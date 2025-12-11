use donut_core::cell::*;
use donut_core::table::*;
use donut_util::println;
use std::rc::Rc;

type R = f32;
type RCoord = Vec<R>;

fn lerp(a: R, b: R, r: R) -> R {
    a * (1.0 - r) + b * r
}

pub struct Renderer {
    context: web_sys::CanvasRenderingContext2d,
    prim_table: Rc<PrimTable>,
}

impl Renderer {
    pub fn new(context: web_sys::CanvasRenderingContext2d, prim_table: Rc<PrimTable>) -> Self {
        Self {
            context,
            prim_table,
        }
    }

    pub fn push(&self) {
        self.context.save();
    }

    pub fn pop(&self) {
        self.context.restore();
    }

    pub fn set_fill_color(&self, color: (u8, u8, u8, u8)) {
        let (r, g, b, a) = color;
        let str = if a == 255 {
            format!("rgb({} {} {})", r, g, b)
        } else {
            format!("rgb({} {} {} / {})", r, g, b, a as f64 / 255.0)
        };
        self.context.set_fill_style_str(&str);
    }

    pub fn set_stroke_color(&self, color: (u8, u8, u8, u8), width: u32) {
        let (r, g, b, a) = color;
        let str = if a == 255 {
            format!("rgb({} {} {})", r, g, b)
        } else {
            format!("rgb({} {} {} / {})", r, g, b, a as f64 / 255.0)
        };
        self.context.set_stroke_style_str(&str);
        self.context.set_line_width(width as f64);
    }

    pub fn offset(&self, x: u32, y: u32) {
        self.context.translate(x as f64, y as f64).unwrap();
    }

    pub fn rect(&self, x: u32, y: u32, width: u32, height: u32) {
        self.context
            .fill_rect(x as f64, y as f64, width as f64, height as f64);
    }

    pub fn frame(&self, x: u32, y: u32, width: u32, height: u32) {
        self.context.set_stroke_style_str("cyan");
        self.context.set_line_width(1.0);
        self.context.begin_path();
        self.context.rect(
            x as f64 + 0.5,
            y as f64 + 0.5,
            width as f64 - 1.0,
            height as f64 - 1.0,
        );
        self.context.stroke();
    }

    pub fn circle(&self, x: u32, y: u32, radius: u32) {
        self.context.begin_path();
        self.context
            .arc(
                x as f64,
                y as f64,
                radius as f64,
                0.0,
                std::f64::consts::PI * 2.0,
            )
            .unwrap();
        self.context.fill();
    }

    pub fn circle_r(&self, x: R, y: R, radius: R) {
        self.context.begin_path();
        self.context
            .arc(
                x as f64,
                y as f64,
                radius as f64,
                0.0,
                std::f64::consts::PI * 2.0,
            )
            .unwrap();
        self.context.fill();
    }
}
