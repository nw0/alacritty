//! ANSI Terminal Stream Parsing.

use serde::{Deserialize, Serialize};

pub use vte::ansi::{
    Attr, CharsetIndex, ClearMode, Color, CursorStyle, Handler, LineClearMode, Mode, NamedColor,
    Processor, StandardCharset, TabulationClearMode,
};

use crate::term::color::{Rgb, VteRgb};

/// Describes shape of cursor: remote derive from `vte`
#[derive(Deserialize)]
#[serde(remote = "CursorStyle")]
pub enum CursorStyleDef {
    /// Cursor is a block like `▒`.
    Block,

    /// Cursor is an underscore like `_`.
    Underline,

    /// Cursor is a vertical bar `⎸`.
    Beam,

    /// Cursor is a box like `☐`.
    #[serde(skip)]
    HollowBlock,

    /// Invisible cursor.
    #[serde(skip)]
    Hidden,
}

#[derive(Serialize, Deserialize)]
#[serde(remote = "Color")]
pub enum ColorDef {
    #[serde(with = "NamedColorDef")]
    Named(NamedColor),
    #[serde(with = "Rgb")]
    Spec(VteRgb),
    Indexed(u8),
}

/// Standard colors: remote derive from `vte`.
#[derive(Serialize, Deserialize)]
#[serde(remote = "NamedColor")]
pub enum NamedColorDef {
    /// Black.
    Black = 0,
    /// Red.
    Red,
    /// Green.
    Green,
    /// Yellow.
    Yellow,
    /// Blue.
    Blue,
    /// Magenta.
    Magenta,
    /// Cyan.
    Cyan,
    /// White.
    White,
    /// Bright black.
    BrightBlack,
    /// Bright red.
    BrightRed,
    /// Bright green.
    BrightGreen,
    /// Bright yellow.
    BrightYellow,
    /// Bright blue.
    BrightBlue,
    /// Bright magenta.
    BrightMagenta,
    /// Bright cyan.
    BrightCyan,
    /// Bright white.
    BrightWhite,
    /// The foreground color.
    Foreground = 256,
    /// The background color.
    Background,
    /// Color for the cursor itself.
    Cursor,
    /// Dim black.
    DimBlack,
    /// Dim red.
    DimRed,
    /// Dim green.
    DimGreen,
    /// Dim yellow.
    DimYellow,
    /// Dim blue.
    DimBlue,
    /// Dim magenta.
    DimMagenta,
    /// Dim cyan.
    DimCyan,
    /// Dim white.
    DimWhite,
    /// The bright foreground color.
    BrightForeground,
    /// Dim foreground.
    DimForeground,
}
