use std::cell::RefCell;
use std::rc::Rc;

use donut_core::cell::*;
use donut_core::common::*;
use donut_core::free_cell::FreeCell;
use donut_layout::layout_solver::LayoutSolver;
use donut_renderer::geometry::{Geometry, R};
use donut_renderer::prim_table::PrimTable;
use donut_renderer::render::Renderer;

pub struct App {
    canvas: web_sys::HtmlCanvasElement,
    context: web_sys::CanvasRenderingContext2d,
    mouse: Rc<RefCell<(f64, f64)>>,
    cell: Geometry,
    table: PrimTable,
    t: R,
}

impl App {
    pub fn new(
        canvas: web_sys::HtmlCanvasElement,
        context: web_sys::CanvasRenderingContext2d,
        mouse: Rc<RefCell<(f64, f64)>>,
    ) -> Self {
        let input = r#"
            [gray(40)]
            u: *
            [hsv(0.6, 1, 1)]
            x: u → u
            [hsv(0.1, 1.0, 1)]
            m: x x → x
            [hsv(0.2, 1.0, 1)]
            a: m x; m → x m; m
            [gray(200)]
            chl: (x m; m) x → x m x; m x
            [gray(200)]
            chr: x m x; x m → x (m x; m)
            aaa =
                a x; m ;;
                chl; m ;;
                x m x; a ;;
                chr; m ;;
                x a; m
            [gray(200)]
            ch0: m x x; x m → m m
            [gray(200)]
            ch1: m m → x x m; m x

            [gray(200)]
            kl: (m x; m) x → m x x; m x
            [gray(200)]
            kr: x x m; x m → x (x m; m)
            oao =
                kl; m ;;
                m x x; a ;;
                (ch0 ;; ch1); m ;;
                x x m; a ;;
                kr; m

            [gray(255)]
            pentagon: aaa → oao
            result = pentagon
        "#;
        let symbol_table = donut_lang::load::load(input).unwrap();

        let mut table = PrimTable::new();
        for (i, e) in symbol_table.elements.iter().enumerate() {
            let prim = Prim::new(i as PrimId);
            table.insert(prim, &e.name, e.cell.pure.dim().in_space, e.color);
        }

        let cell = symbol_table.elements.last().unwrap().cell.clone();
        let mut f = LayoutSolver::new();
        let cell = f.from_free(cell);
        let sol = f.solve(&cell);
        let cell = sol.convert(&cell);
        // log::debug!("Cell: {}", cell);
        let mut cell = cell.render();
        while cell.max.len() < 4 {
            cell.shift(&Q::from(0), &Q::from(1));
        }
        let cell = Geometry::from(&cell);
        log::debug!("Geometry: {:?}", cell.size);

        Self {
            canvas,
            context,
            mouse,
            cell,
            table,
            t: 0.0,
        }
    }

    pub fn step(&mut self) {
        let width = self.canvas.width() as f64;
        let height = self.canvas.height() as f64;
        self.context.set_fill_style_str("rgb(40 40 40)");
        self.context.fill_rect(0.0, 0.0, width, height);
        self.context.set_fill_style_str("rgb(50 50 50)");
        self.context.fill_rect(50.0, 50.0, 500.0, 400.0);

        let size = self.cell.size.clone();
        let slicer_x = size[0] + 200.0;
        let slicer_y = 100.0;

        let mouse = *self.mouse.borrow();
        self.context.save();
        self.context.translate(100.0, 100.0).unwrap();
        let renderer = Renderer::new(self.context.clone());
        let x = (mouse.0 - slicer_x).min(size[2]).max(0.0);
        let y = (mouse.1 - slicer_y).min(size[3]).max(0.0);
        let rc = &self.cell;
        let rc = rc.sliced(y);
        let rc = rc.sliced(x);
        renderer.cell(&rc, &self.table);
        self.context.restore();
        self.context.save();
        self.context.translate(slicer_x, slicer_y).unwrap();
        let rc = &self.cell;
        let rc = rc.squashed();
        let rc = rc.squashed();
        renderer.cell(&rc, &self.table);
        self.context.restore();

        self.t += 0.03;
    }
}
