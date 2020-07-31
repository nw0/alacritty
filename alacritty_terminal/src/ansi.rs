//! ANSI Terminal Stream Parsing.

use std::io;
use std::str;

use log::debug;
pub use vte::ansi::{
    Attr, CharsetIndex, ClearMode, Color, CursorStyle, Handler, LineClearMode, Mode, NamedColor,
    StandardCharset, TabulationClearMode,
};

use crate::term::color::{Rgb, VteRgb};

/// Parse colors in XParseColor format.
fn xparse_color(color: &[u8]) -> Option<Rgb> {
    if !color.is_empty() && color[0] == b'#' {
        parse_legacy_color(&color[1..])
    } else if color.len() >= 4 && &color[..4] == b"rgb:" {
        parse_rgb_color(&color[4..])
    } else {
        None
    }
}

/// Parse colors in `rgb:r(rrr)/g(ggg)/b(bbb)` format.
fn parse_rgb_color(color: &[u8]) -> Option<Rgb> {
    let colors = str::from_utf8(color).ok()?.split('/').collect::<Vec<_>>();

    if colors.len() != 3 {
        return None;
    }

    // Scale values instead of filling with `0`s.
    let scale = |input: &str| {
        let max = u32::pow(16, input.len() as u32) - 1;
        let value = u32::from_str_radix(input, 16).ok()?;
        Some((255 * value / max) as u8)
    };

    Some(Rgb { r: scale(colors[0])?, g: scale(colors[1])?, b: scale(colors[2])? })
}

/// Parse colors in `#r(rrr)g(ggg)b(bbb)` format.
fn parse_legacy_color(color: &[u8]) -> Option<Rgb> {
    let item_len = color.len() / 3;

    // Truncate/Fill to two byte precision.
    let color_from_slice = |slice: &[u8]| {
        let col = usize::from_str_radix(str::from_utf8(slice).ok()?, 16).ok()? << 4;
        Some((col >> (4 * slice.len().saturating_sub(1))) as u8)
    };

    Some(Rgb {
        r: color_from_slice(&color[0..item_len])?,
        g: color_from_slice(&color[item_len..item_len * 2])?,
        b: color_from_slice(&color[item_len * 2..])?,
    })
}

fn parse_number(input: &[u8]) -> Option<u8> {
    if input.is_empty() {
        return None;
    }
    let mut num: u8 = 0;
    for c in input {
        let c = *c as char;
        if let Some(digit) = c.to_digit(10) {
            num = match num.checked_mul(10).and_then(|v| v.checked_add(digit as u8)) {
                Some(v) => v,
                None => return None,
            }
        } else {
            return None;
        }
    }
    Some(num)
}

/// The processor wraps a `vte::Parser` to ultimately call methods on a Handler.
pub struct Processor {
    state: ProcessorState,
    parser: vte::Parser,
}

/// Internal state for VTE processor.
struct ProcessorState {
    preceding_char: Option<char>,
}

/// Helper type that implements `vte::Perform`.
///
/// Processor creates a Performer when running advance and passes the Performer
/// to `vte::Parser`.
struct Performer<'a, H: Handler, W: io::Write> {
    state: &'a mut ProcessorState,
    handler: &'a mut H,
    writer: &'a mut W,
}

impl<'a, H: Handler + 'a, W: io::Write> Performer<'a, H, W> {
    /// Create a performer.
    #[inline]
    pub fn new<'b>(
        state: &'b mut ProcessorState,
        handler: &'b mut H,
        writer: &'b mut W,
    ) -> Performer<'b, H, W> {
        Performer { state, handler, writer }
    }
}

impl Default for Processor {
    fn default() -> Processor {
        Processor { state: ProcessorState { preceding_char: None }, parser: vte::Parser::new() }
    }
}

impl Processor {
    pub fn new() -> Processor {
        Default::default()
    }

    #[inline]
    pub fn advance<H, W>(&mut self, handler: &mut H, byte: u8, writer: &mut W)
    where
        H: Handler,
        W: io::Write,
    {
        let mut performer = Performer::new(&mut self.state, handler, writer);
        self.parser.advance(&mut performer, byte);
    }
}

impl<'a, H, W> vte::Perform for Performer<'a, H, W>
where
    H: Handler + 'a,
    W: io::Write + 'a,
{
    #[inline]
    fn print(&mut self, c: char) {
        self.handler.input(c);
        self.state.preceding_char = Some(c);
    }

    #[inline]
    fn execute(&mut self, byte: u8) {
        match byte {
            C0::HT => self.handler.put_tab(1),
            C0::BS => self.handler.backspace(),
            C0::CR => self.handler.carriage_return(),
            C0::LF | C0::VT | C0::FF => self.handler.linefeed(),
            C0::BEL => self.handler.bell(),
            C0::SUB => self.handler.substitute(),
            C0::SI => self.handler.set_active_charset(CharsetIndex::G0),
            C0::SO => self.handler.set_active_charset(CharsetIndex::G1),
            _ => debug!("[unhandled] execute byte={:02x}", byte),
        }
    }

    #[inline]
    fn hook(&mut self, params: &[i64], intermediates: &[u8], ignore: bool, _c: char) {
        debug!(
            "[unhandled hook] params={:?}, ints: {:?}, ignore: {:?}",
            params, intermediates, ignore
        );
    }

    #[inline]
    fn put(&mut self, byte: u8) {
        debug!("[unhandled put] byte={:?}", byte);
    }

    #[inline]
    fn unhook(&mut self) {
        debug!("[unhandled unhook]");
    }

    // TODO replace OSC parsing with parser combinators.
    #[inline]
    fn osc_dispatch(&mut self, params: &[&[u8]], bell_terminated: bool) {
        let writer = &mut self.writer;
        let terminator = if bell_terminated { "\x07" } else { "\x1b\\" };

        fn unhandled(params: &[&[u8]]) {
            let mut buf = String::new();
            for items in params {
                buf.push_str("[");
                for item in *items {
                    buf.push_str(&format!("{:?},", *item as char));
                }
                buf.push_str("],");
            }
            debug!("[unhandled osc_dispatch]: [{}] at line {}", &buf, line!());
        }

        if params.is_empty() || params[0].is_empty() {
            return;
        }

        match params[0] {
            // Set window title.
            b"0" | b"2" => {
                if params.len() >= 2 {
                    let title = params[1..]
                        .iter()
                        .flat_map(|x| str::from_utf8(x))
                        .collect::<Vec<&str>>()
                        .join(";")
                        .trim()
                        .to_owned();
                    self.handler.set_title(Some(title));
                    return;
                }
                unhandled(params);
            },

            // Set color index.
            b"4" => {
                if params.len() > 1 && params.len() % 2 != 0 {
                    for chunk in params[1..].chunks(2) {
                        let index = parse_number(chunk[0]);
                        let color = xparse_color(chunk[1]);
                        if let (Some(i), Some(c)) = (index, color) {
                            self.handler.set_color(i as usize, c.into());
                            return;
                        }
                    }
                }
                unhandled(params);
            },

            // Get/set Foreground, Background, Cursor colors.
            b"10" | b"11" | b"12" => {
                if params.len() >= 2 {
                    if let Some(mut dynamic_code) = parse_number(params[0]) {
                        for param in &params[1..] {
                            // 10 is the first dynamic color, also the foreground.
                            let offset = dynamic_code as usize - 10;
                            let index = NamedColor::Foreground as usize + offset;

                            // End of setting dynamic colors.
                            if index > NamedColor::Cursor as usize {
                                unhandled(params);
                                break;
                            }

                            if let Some(color) = xparse_color(param) {
                                self.handler.set_color(index, color.into());
                            } else if param == b"?" {
                                self.handler.dynamic_color_sequence(
                                    writer,
                                    dynamic_code,
                                    index,
                                    terminator,
                                );
                            } else {
                                unhandled(params);
                            }
                            dynamic_code += 1;
                        }
                        return;
                    }
                }
                unhandled(params);
            },

            // Set cursor style.
            b"50" => {
                if params.len() >= 2
                    && params[1].len() >= 13
                    && params[1][0..12] == *b"CursorShape="
                {
                    let style = match params[1][12] as char {
                        '0' => CursorStyle::Block,
                        '1' => CursorStyle::Beam,
                        '2' => CursorStyle::Underline,
                        _ => return unhandled(params),
                    };
                    self.handler.set_cursor_style(Some(style));
                    return;
                }
                unhandled(params);
            },

            // Set clipboard.
            b"52" => {
                if params.len() < 3 {
                    return unhandled(params);
                }

                let clipboard = params[1].get(0).unwrap_or(&b'c');
                match params[2] {
                    b"?" => self.handler.clipboard_load(*clipboard, terminator),
                    base64 => self.handler.clipboard_store(*clipboard, base64),
                }
            },

            // Reset color index.
            b"104" => {
                // Reset all color indexes when no parameters are given.
                if params.len() == 1 {
                    for i in 0..256 {
                        self.handler.reset_color(i);
                    }
                    return;
                }

                // Reset color indexes given as parameters.
                for param in &params[1..] {
                    match parse_number(param) {
                        Some(index) => self.handler.reset_color(index as usize),
                        None => unhandled(params),
                    }
                }
            },

            // Reset foreground color.
            b"110" => self.handler.reset_color(NamedColor::Foreground as usize),

            // Reset background color.
            b"111" => self.handler.reset_color(NamedColor::Background as usize),

            // Reset text cursor color.
            b"112" => self.handler.reset_color(NamedColor::Cursor as usize),

            _ => unhandled(params),
        }
    }

    #[allow(clippy::cognitive_complexity)]
    #[inline]
    fn csi_dispatch(
        &mut self,
        args: &[i64],
        intermediates: &[u8],
        has_ignored_intermediates: bool,
        action: char,
    ) {
        macro_rules! unhandled {
            () => {{
                debug!(
                    "[Unhandled CSI] action={:?}, args={:?}, intermediates={:?}",
                    action, args, intermediates
                );
            }};
        }

        macro_rules! arg_or_default {
            (idx: $idx:expr, default: $default:expr) => {
                args.get($idx).copied().filter(|&v| v != 0).unwrap_or($default)
            };
        }

        if has_ignored_intermediates || intermediates.len() > 1 {
            unhandled!();
            return;
        }

        let handler = &mut self.handler;
        let writer = &mut self.writer;

        match (action, intermediates.get(0)) {
            ('@', None) => handler.insert_blank(arg_or_default!(idx: 0, default: 1) as usize),
            ('A', None) => {
                handler.move_up(arg_or_default!(idx: 0, default: 1) as usize);
            },
            ('B', None) | ('e', None) => {
                handler.move_down(arg_or_default!(idx: 0, default: 1) as usize)
            },
            ('b', None) => {
                if let Some(c) = self.state.preceding_char {
                    for _ in 0..arg_or_default!(idx: 0, default: 1) {
                        handler.input(c);
                    }
                } else {
                    debug!("tried to repeat with no preceding char");
                }
            },
            ('C', None) | ('a', None) => {
                handler.move_forward(arg_or_default!(idx: 0, default: 1) as usize)
            },
            ('c', intermediate) if arg_or_default!(idx: 0, default: 0) == 0 => {
                handler.identify_terminal(writer, intermediate.map(|&i| i as char))
            },
            ('D', None) => handler.move_backward(arg_or_default!(idx: 0, default: 1) as usize),
            ('d', None) => handler.goto_line(arg_or_default!(idx: 0, default: 1) as usize - 1),
            ('E', None) => handler.move_down_and_cr(arg_or_default!(idx: 0, default: 1) as usize),
            ('F', None) => handler.move_up_and_cr(arg_or_default!(idx: 0, default: 1) as usize),
            ('G', None) | ('`', None) => {
                handler.goto_col(arg_or_default!(idx: 0, default: 1) as usize - 1)
            },
            ('g', None) => {
                let mode = match arg_or_default!(idx: 0, default: 0) {
                    0 => TabulationClearMode::Current,
                    3 => TabulationClearMode::All,
                    _ => {
                        unhandled!();
                        return;
                    },
                };

                handler.clear_tabs(mode);
            },
            ('H', None) | ('f', None) => {
                let y = arg_or_default!(idx: 0, default: 1) as usize;
                let x = arg_or_default!(idx: 1, default: 1) as usize;
                handler.goto(y - 1, x - 1);
            },
            ('h', intermediate) => {
                for arg in args {
                    match Mode::from_primitive(intermediate, *arg) {
                        Some(mode) => handler.set_mode(mode),
                        None => unhandled!(),
                    }
                }
            },
            ('I', None) => handler.move_forward_tabs(arg_or_default!(idx: 0, default: 1)),
            ('J', None) => {
                let mode = match arg_or_default!(idx: 0, default: 0) {
                    0 => ClearMode::Below,
                    1 => ClearMode::Above,
                    2 => ClearMode::All,
                    3 => ClearMode::Saved,
                    _ => {
                        unhandled!();
                        return;
                    },
                };

                handler.clear_screen(mode);
            },
            ('K', None) => {
                let mode = match arg_or_default!(idx: 0, default: 0) {
                    0 => LineClearMode::Right,
                    1 => LineClearMode::Left,
                    2 => LineClearMode::All,
                    _ => {
                        unhandled!();
                        return;
                    },
                };

                handler.clear_line(mode);
            },
            ('L', None) => handler.insert_blank_lines(arg_or_default!(idx: 0, default: 1) as usize),
            ('l', intermediate) => {
                for arg in args {
                    match Mode::from_primitive(intermediate, *arg) {
                        Some(mode) => handler.unset_mode(mode),
                        None => unhandled!(),
                    }
                }
            },
            ('M', None) => handler.delete_lines(arg_or_default!(idx: 0, default: 1) as usize),
            ('m', None) => {
                if args.is_empty() {
                    handler.terminal_attribute(Attr::Reset);
                } else {
                    for attr in attrs_from_sgr_parameters(args) {
                        match attr {
                            Some(attr) => handler.terminal_attribute(attr),
                            None => unhandled!(),
                        }
                    }
                }
            },
            ('n', None) => {
                handler.device_status(writer, arg_or_default!(idx: 0, default: 0) as usize)
            },
            ('P', None) => handler.delete_chars(arg_or_default!(idx: 0, default: 1) as usize),
            ('q', Some(b' ')) => {
                // DECSCUSR (CSI Ps SP q) -- Set Cursor Style.
                let style = match arg_or_default!(idx: 0, default: 0) {
                    0 => None,
                    1 | 2 => Some(CursorStyle::Block),
                    3 | 4 => Some(CursorStyle::Underline),
                    5 | 6 => Some(CursorStyle::Beam),
                    _ => {
                        unhandled!();
                        return;
                    },
                };

                handler.set_cursor_style(style);
            },
            ('r', None) => {
                let top = arg_or_default!(idx: 0, default: 1) as usize;
                let bottom = args.get(1).map(|&b| b as usize).filter(|&b| b != 0);

                handler.set_scrolling_region(top, bottom);
            },
            ('S', None) => handler.scroll_up(arg_or_default!(idx: 0, default: 1) as usize),
            ('s', None) => handler.save_cursor_position(),
            ('T', None) => handler.scroll_down(arg_or_default!(idx: 0, default: 1) as usize),
            ('t', None) => match arg_or_default!(idx: 0, default: 1) as usize {
                22 => handler.push_title(),
                23 => handler.pop_title(),
                _ => unhandled!(),
            },
            ('u', None) => handler.restore_cursor_position(),
            ('X', None) => handler.erase_chars(arg_or_default!(idx: 0, default: 1) as usize),
            ('Z', None) => handler.move_backward_tabs(arg_or_default!(idx: 0, default: 1)),
            _ => unhandled!(),
        }
    }

    #[inline]
    fn esc_dispatch(&mut self, intermediates: &[u8], _ignore: bool, byte: u8) {
        macro_rules! unhandled {
            () => {{
                debug!(
                    "[unhandled] esc_dispatch ints={:?}, byte={:?} ({:02x})",
                    intermediates, byte as char, byte
                );
            }};
        }

        macro_rules! configure_charset {
            ($charset:path, $intermediate:expr) => {{
                let index: CharsetIndex = match $intermediate {
                    Some(b'(') => CharsetIndex::G0,
                    Some(b')') => CharsetIndex::G1,
                    Some(b'*') => CharsetIndex::G2,
                    Some(b'+') => CharsetIndex::G3,
                    _ => {
                        unhandled!();
                        return;
                    },
                };
                self.handler.configure_charset(index, $charset)
            }};
        }

        match (byte, intermediates.get(0)) {
            (b'B', intermediate) => configure_charset!(StandardCharset::Ascii, intermediate),
            (b'D', None) => self.handler.linefeed(),
            (b'E', None) => {
                self.handler.linefeed();
                self.handler.carriage_return();
            },
            (b'H', None) => self.handler.set_horizontal_tabstop(),
            (b'M', None) => self.handler.reverse_index(),
            (b'Z', None) => self.handler.identify_terminal(self.writer, None),
            (b'c', None) => self.handler.reset_state(),
            (b'0', intermediate) => {
                configure_charset!(StandardCharset::SpecialCharacterAndLineDrawing, intermediate)
            },
            (b'7', None) => self.handler.save_cursor_position(),
            (b'8', Some(b'#')) => self.handler.decaln(),
            (b'8', None) => self.handler.restore_cursor_position(),
            (b'=', None) => self.handler.set_keypad_application_mode(),
            (b'>', None) => self.handler.unset_keypad_application_mode(),
            // String terminator, do nothing (parser handles as string terminator).
            (b'\\', None) => (),
            _ => unhandled!(),
        }
    }
}

fn attrs_from_sgr_parameters(parameters: &[i64]) -> Vec<Option<Attr>> {
    let mut i = 0;
    let mut attrs = Vec::with_capacity(parameters.len());
    loop {
        if i >= parameters.len() {
            break;
        }

        let attr = match parameters[i] {
            0 => Some(Attr::Reset),
            1 => Some(Attr::Bold),
            2 => Some(Attr::Dim),
            3 => Some(Attr::Italic),
            4 => Some(Attr::Underline),
            5 => Some(Attr::BlinkSlow),
            6 => Some(Attr::BlinkFast),
            7 => Some(Attr::Reverse),
            8 => Some(Attr::Hidden),
            9 => Some(Attr::Strike),
            21 => Some(Attr::CancelBold),
            22 => Some(Attr::CancelBoldDim),
            23 => Some(Attr::CancelItalic),
            24 => Some(Attr::CancelUnderline),
            25 => Some(Attr::CancelBlink),
            27 => Some(Attr::CancelReverse),
            28 => Some(Attr::CancelHidden),
            29 => Some(Attr::CancelStrike),
            30 => Some(Attr::Foreground(Color::Named(NamedColor::Black))),
            31 => Some(Attr::Foreground(Color::Named(NamedColor::Red))),
            32 => Some(Attr::Foreground(Color::Named(NamedColor::Green))),
            33 => Some(Attr::Foreground(Color::Named(NamedColor::Yellow))),
            34 => Some(Attr::Foreground(Color::Named(NamedColor::Blue))),
            35 => Some(Attr::Foreground(Color::Named(NamedColor::Magenta))),
            36 => Some(Attr::Foreground(Color::Named(NamedColor::Cyan))),
            37 => Some(Attr::Foreground(Color::Named(NamedColor::White))),
            38 => {
                let mut start = 0;
                if let Some(color) = parse_sgr_color(&parameters[i..], &mut start) {
                    i += start;
                    Some(Attr::Foreground(color))
                } else {
                    None
                }
            },
            39 => Some(Attr::Foreground(Color::Named(NamedColor::Foreground))),
            40 => Some(Attr::Background(Color::Named(NamedColor::Black))),
            41 => Some(Attr::Background(Color::Named(NamedColor::Red))),
            42 => Some(Attr::Background(Color::Named(NamedColor::Green))),
            43 => Some(Attr::Background(Color::Named(NamedColor::Yellow))),
            44 => Some(Attr::Background(Color::Named(NamedColor::Blue))),
            45 => Some(Attr::Background(Color::Named(NamedColor::Magenta))),
            46 => Some(Attr::Background(Color::Named(NamedColor::Cyan))),
            47 => Some(Attr::Background(Color::Named(NamedColor::White))),
            48 => {
                let mut start = 0;
                if let Some(color) = parse_sgr_color(&parameters[i..], &mut start) {
                    i += start;
                    Some(Attr::Background(color))
                } else {
                    None
                }
            },
            49 => Some(Attr::Background(Color::Named(NamedColor::Background))),
            90 => Some(Attr::Foreground(Color::Named(NamedColor::BrightBlack))),
            91 => Some(Attr::Foreground(Color::Named(NamedColor::BrightRed))),
            92 => Some(Attr::Foreground(Color::Named(NamedColor::BrightGreen))),
            93 => Some(Attr::Foreground(Color::Named(NamedColor::BrightYellow))),
            94 => Some(Attr::Foreground(Color::Named(NamedColor::BrightBlue))),
            95 => Some(Attr::Foreground(Color::Named(NamedColor::BrightMagenta))),
            96 => Some(Attr::Foreground(Color::Named(NamedColor::BrightCyan))),
            97 => Some(Attr::Foreground(Color::Named(NamedColor::BrightWhite))),
            100 => Some(Attr::Background(Color::Named(NamedColor::BrightBlack))),
            101 => Some(Attr::Background(Color::Named(NamedColor::BrightRed))),
            102 => Some(Attr::Background(Color::Named(NamedColor::BrightGreen))),
            103 => Some(Attr::Background(Color::Named(NamedColor::BrightYellow))),
            104 => Some(Attr::Background(Color::Named(NamedColor::BrightBlue))),
            105 => Some(Attr::Background(Color::Named(NamedColor::BrightMagenta))),
            106 => Some(Attr::Background(Color::Named(NamedColor::BrightCyan))),
            107 => Some(Attr::Background(Color::Named(NamedColor::BrightWhite))),
            _ => None,
        };

        attrs.push(attr);

        i += 1;
    }
    attrs
}

/// Parse a color specifier from list of attributes.
fn parse_sgr_color(attrs: &[i64], i: &mut usize) -> Option<Color> {
    if attrs.len() < 2 {
        return None;
    }

    match attrs[*i + 1] {
        2 => {
            // RGB color spec.
            if attrs.len() < 5 {
                debug!("Expected RGB color spec; got {:?}", attrs);
                return None;
            }

            let r = attrs[*i + 2];
            let g = attrs[*i + 3];
            let b = attrs[*i + 4];

            *i += 4;

            let range = 0..256;
            if !range.contains(&r) || !range.contains(&g) || !range.contains(&b) {
                debug!("Invalid RGB color spec: ({}, {}, {})", r, g, b);
                return None;
            }

            Some(Color::Spec(VteRgb { r: r as u8, g: g as u8, b: b as u8 }))
        },
        5 => {
            if attrs.len() < 3 {
                debug!("Expected color index; got {:?}", attrs);
                None
            } else {
                *i += 2;
                let idx = attrs[*i];
                match idx {
                    0..=255 => Some(Color::Indexed(idx as u8)),
                    _ => {
                        debug!("Invalid color index: {}", idx);
                        None
                    },
                }
            }
        },
        _ => {
            debug!("Unexpected color attr: {}", attrs[*i + 1]);
            None
        },
    }
}

/// C0 set of 7-bit control characters (from ANSI X3.4-1977).
#[allow(non_snake_case)]
pub mod C0 {
    /// Null filler, terminal should ignore this character.
    pub const NUL: u8 = 0x00;
    /// Start of Header.
    pub const SOH: u8 = 0x01;
    /// Start of Text, implied end of header.
    pub const STX: u8 = 0x02;
    /// End of Text, causes some terminal to respond with ACK or NAK.
    pub const ETX: u8 = 0x03;
    /// End of Transmission.
    pub const EOT: u8 = 0x04;
    /// Enquiry, causes terminal to send ANSWER-BACK ID.
    pub const ENQ: u8 = 0x05;
    /// Acknowledge, usually sent by terminal in response to ETX.
    pub const ACK: u8 = 0x06;
    /// Bell, triggers the bell, buzzer, or beeper on the terminal.
    pub const BEL: u8 = 0x07;
    /// Backspace, can be used to define overstruck characters.
    pub const BS: u8 = 0x08;
    /// Horizontal Tabulation, move to next predetermined position.
    pub const HT: u8 = 0x09;
    /// Linefeed, move to same position on next line (see also NL).
    pub const LF: u8 = 0x0A;
    /// Vertical Tabulation, move to next predetermined line.
    pub const VT: u8 = 0x0B;
    /// Form Feed, move to next form or page.
    pub const FF: u8 = 0x0C;
    /// Carriage Return, move to first character of current line.
    pub const CR: u8 = 0x0D;
    /// Shift Out, switch to G1 (other half of character set).
    pub const SO: u8 = 0x0E;
    /// Shift In, switch to G0 (normal half of character set).
    pub const SI: u8 = 0x0F;
    /// Data Link Escape, interpret next control character specially.
    pub const DLE: u8 = 0x10;
    /// (DC1) Terminal is allowed to resume transmitting.
    pub const XON: u8 = 0x11;
    /// Device Control 2, causes ASR-33 to activate paper-tape reader.
    pub const DC2: u8 = 0x12;
    /// (DC2) Terminal must pause and refrain from transmitting.
    pub const XOFF: u8 = 0x13;
    /// Device Control 4, causes ASR-33 to deactivate paper-tape reader.
    pub const DC4: u8 = 0x14;
    /// Negative Acknowledge, used sometimes with ETX and ACK.
    pub const NAK: u8 = 0x15;
    /// Synchronous Idle, used to maintain timing in Sync communication.
    pub const SYN: u8 = 0x16;
    /// End of Transmission block.
    pub const ETB: u8 = 0x17;
    /// Cancel (makes VT100 abort current escape sequence if any).
    pub const CAN: u8 = 0x18;
    /// End of Medium.
    pub const EM: u8 = 0x19;
    /// Substitute (VT100 uses this to display parity errors).
    pub const SUB: u8 = 0x1A;
    /// Prefix to an escape sequence.
    pub const ESC: u8 = 0x1B;
    /// File Separator.
    pub const FS: u8 = 0x1C;
    /// Group Separator.
    pub const GS: u8 = 0x1D;
    /// Record Separator (sent by VT132 in block-transfer mode).
    pub const RS: u8 = 0x1E;
    /// Unit Separator.
    pub const US: u8 = 0x1F;
    /// Delete, should be ignored by terminal.
    pub const DEL: u8 = 0x7f;
}

// Tests for parsing escape sequences.
//
// Byte sequences used in these tests are recording of pty stdout.
#[cfg(test)]
mod tests {
    use super::{
        parse_number, xparse_color, Attr, CharsetIndex, Color, Handler, Processor, StandardCharset,
    };
    use crate::term::color::Rgb;
    use std::io;

    struct MockHandler {
        index: CharsetIndex,
        charset: StandardCharset,
        attr: Option<Attr>,
        identity_reported: bool,
    }

    impl Handler for MockHandler {
        fn terminal_attribute(&mut self, attr: Attr) {
            self.attr = Some(attr);
        }

        fn configure_charset(&mut self, index: CharsetIndex, charset: StandardCharset) {
            self.index = index;
            self.charset = charset;
        }

        fn set_active_charset(&mut self, index: CharsetIndex) {
            self.index = index;
        }

        fn identify_terminal<W: io::Write>(&mut self, _: &mut W, _intermediate: Option<char>) {
            self.identity_reported = true;
        }

        fn reset_state(&mut self) {
            *self = Self::default();
        }
    }

    impl Default for MockHandler {
        fn default() -> MockHandler {
            MockHandler {
                index: CharsetIndex::G0,
                charset: StandardCharset::Ascii,
                attr: None,
                identity_reported: false,
            }
        }
    }

    #[test]
    fn parse_control_attribute() {
        static BYTES: &[u8] = &[0x1b, b'[', b'1', b'm'];

        let mut parser = Processor::new();
        let mut handler = MockHandler::default();

        for byte in &BYTES[..] {
            parser.advance(&mut handler, *byte, &mut io::sink());
        }

        assert_eq!(handler.attr, Some(Attr::Bold));
    }

    #[test]
    fn parse_terminal_identity_csi() {
        let bytes: &[u8] = &[0x1b, b'[', b'1', b'c'];

        let mut parser = Processor::new();
        let mut handler = MockHandler::default();

        for byte in &bytes[..] {
            parser.advance(&mut handler, *byte, &mut io::sink());
        }

        assert!(!handler.identity_reported);
        handler.reset_state();

        let bytes: &[u8] = &[0x1b, b'[', b'c'];

        for byte in &bytes[..] {
            parser.advance(&mut handler, *byte, &mut io::sink());
        }

        assert!(handler.identity_reported);
        handler.reset_state();

        let bytes: &[u8] = &[0x1b, b'[', b'0', b'c'];

        for byte in &bytes[..] {
            parser.advance(&mut handler, *byte, &mut io::sink());
        }

        assert!(handler.identity_reported);
    }

    #[test]
    fn parse_terminal_identity_esc() {
        let bytes: &[u8] = &[0x1b, b'Z'];

        let mut parser = Processor::new();
        let mut handler = MockHandler::default();

        for byte in &bytes[..] {
            parser.advance(&mut handler, *byte, &mut io::sink());
        }

        assert!(handler.identity_reported);
        handler.reset_state();

        let bytes: &[u8] = &[0x1b, b'#', b'Z'];

        let mut parser = Processor::new();
        let mut handler = MockHandler::default();

        for byte in &bytes[..] {
            parser.advance(&mut handler, *byte, &mut io::sink());
        }

        assert!(!handler.identity_reported);
        handler.reset_state();
    }

    #[test]
    fn parse_truecolor_attr() {
        static BYTES: &[u8] = &[
            0x1b, b'[', b'3', b'8', b';', b'2', b';', b'1', b'2', b'8', b';', b'6', b'6', b';',
            b'2', b'5', b'5', b'm',
        ];

        let mut parser = Processor::new();
        let mut handler = MockHandler::default();

        for byte in &BYTES[..] {
            parser.advance(&mut handler, *byte, &mut io::sink());
        }

        let spec = Rgb { r: 128, g: 66, b: 255 };

        assert_eq!(handler.attr, Some(Attr::Foreground(Color::Spec(spec.into()))));
    }

    /// No exactly a test; useful for debugging.
    #[test]
    fn parse_zsh_startup() {
        static BYTES: &[u8] = &[
            0x1b, b'[', b'1', b'm', 0x1b, b'[', b'7', b'm', b'%', 0x1b, b'[', b'2', b'7', b'm',
            0x1b, b'[', b'1', b'm', 0x1b, b'[', b'0', b'm', b' ', b' ', b' ', b' ', b' ', b' ',
            b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ',
            b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ',
            b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ',
            b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ',
            b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ',
            b' ', b' ', b' ', b'\r', b' ', b'\r', b'\r', 0x1b, b'[', b'0', b'm', 0x1b, b'[', b'2',
            b'7', b'm', 0x1b, b'[', b'2', b'4', b'm', 0x1b, b'[', b'J', b'j', b'w', b'i', b'l',
            b'm', b'@', b'j', b'w', b'i', b'l', b'm', b'-', b'd', b'e', b's', b'k', b' ', 0x1b,
            b'[', b'0', b'1', b';', b'3', b'2', b'm', 0xe2, 0x9e, 0x9c, b' ', 0x1b, b'[', b'0',
            b'1', b';', b'3', b'2', b'm', b' ', 0x1b, b'[', b'3', b'6', b'm', b'~', b'/', b'c',
            b'o', b'd', b'e',
        ];

        let mut handler = MockHandler::default();
        let mut parser = Processor::new();

        for byte in &BYTES[..] {
            parser.advance(&mut handler, *byte, &mut io::sink());
        }
    }

    #[test]
    fn parse_designate_g0_as_line_drawing() {
        static BYTES: &[u8] = &[0x1b, b'(', b'0'];
        let mut parser = Processor::new();
        let mut handler = MockHandler::default();

        for byte in &BYTES[..] {
            parser.advance(&mut handler, *byte, &mut io::sink());
        }

        assert_eq!(handler.index, CharsetIndex::G0);
        assert_eq!(handler.charset, StandardCharset::SpecialCharacterAndLineDrawing);
    }

    #[test]
    fn parse_designate_g1_as_line_drawing_and_invoke() {
        static BYTES: &[u8] = &[0x1b, b')', b'0', 0x0e];
        let mut parser = Processor::new();
        let mut handler = MockHandler::default();

        for byte in &BYTES[..3] {
            parser.advance(&mut handler, *byte, &mut io::sink());
        }

        assert_eq!(handler.index, CharsetIndex::G1);
        assert_eq!(handler.charset, StandardCharset::SpecialCharacterAndLineDrawing);

        let mut handler = MockHandler::default();
        parser.advance(&mut handler, BYTES[3], &mut io::sink());

        assert_eq!(handler.index, CharsetIndex::G1);
    }

    #[test]
    fn parse_valid_rgb_colors() {
        assert_eq!(xparse_color(b"rgb:f/e/d"), Some(Rgb { r: 0xff, g: 0xee, b: 0xdd }));
        assert_eq!(xparse_color(b"rgb:11/aa/ff"), Some(Rgb { r: 0x11, g: 0xaa, b: 0xff }));
        assert_eq!(xparse_color(b"rgb:f/ed1/cb23"), Some(Rgb { r: 0xff, g: 0xec, b: 0xca }));
        assert_eq!(xparse_color(b"rgb:ffff/0/0"), Some(Rgb { r: 0xff, g: 0x0, b: 0x0 }));
    }

    #[test]
    fn parse_valid_legacy_rgb_colors() {
        assert_eq!(xparse_color(b"#1af"), Some(Rgb { r: 0x10, g: 0xa0, b: 0xf0 }));
        assert_eq!(xparse_color(b"#11aaff"), Some(Rgb { r: 0x11, g: 0xaa, b: 0xff }));
        assert_eq!(xparse_color(b"#110aa0ff0"), Some(Rgb { r: 0x11, g: 0xaa, b: 0xff }));
        assert_eq!(xparse_color(b"#1100aa00ff00"), Some(Rgb { r: 0x11, g: 0xaa, b: 0xff }));
    }

    #[test]
    fn parse_invalid_rgb_colors() {
        assert_eq!(xparse_color(b"rgb:0//"), None);
        assert_eq!(xparse_color(b"rgb://///"), None);
    }

    #[test]
    fn parse_invalid_legacy_rgb_colors() {
        assert_eq!(xparse_color(b"#"), None);
        assert_eq!(xparse_color(b"#f"), None);
    }

    #[test]
    fn parse_invalid_number() {
        assert_eq!(parse_number(b"1abc"), None);
    }

    #[test]
    fn parse_valid_number() {
        assert_eq!(parse_number(b"123"), Some(123));
    }

    #[test]
    fn parse_number_too_large() {
        assert_eq!(parse_number(b"321"), None);
    }
}
