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
            [color(30, 30, 30)]
            u: *
            x: u → u
            m: x x → x
            a: m x; m → x m; m
            chl: (x m; m) x → x m x; m x
            chr: x m x; x m → x (m x; m)
            aaa =
                a x; m ;;
                chl; m ;;
                x m x; a ;;
                chr; m ;;
                x a; m
            ch0: m x x; x m → m m
            ch1: m m → x x m; m x

            kl: (m x; m) x → m x x; m x
            kr: x x m; x m → x (x m; m)
            oao =
                kl; m ;;
                m x x; a ;;
                (ch0 ;; ch1); m ;;
                x x m; a ;;
                kr; m

            pentagon: aaa → oao
        "#;
        let symbol_table = donut_lang::load::load(input).unwrap();

        let mut table = PrimTable::new();
        for (i, e) in symbol_table.elements.iter().enumerate() {
            let prim = Prim::new(i as PrimId);
            table.insert(prim, &e.name, e.level, e.color);
        }

        /*
        table.insert(Prim::new(0), "a", 0, (40, 40, 40));
        table.insert(Prim::new(1), "x", 1, (0, 100, 255));
        table.insert(Prim::new(2), "m", 2, (255, 100, 0));
        table.insert(Prim::new(3), "assoc", 3, (255, 255, 0));
        table.insert(Prim::new(4), "chl", 3, (200, 200, 200));
        table.insert(Prim::new(5), "chr", 3, (200, 200, 200));
        table.insert(Prim::new(6), "x0", 3, (200, 200, 200));
        table.insert(Prim::new(7), "x1", 3, (200, 200, 200));
        table.insert(Prim::new(8), "kl", 3, (200, 200, 200));
        table.insert(Prim::new(9), "kr", 3, (200, 200, 200));
        table.insert(Prim::new(10), "pentagon", 4, (255, 255, 255));
        */

        let last_prim = Prim::new(symbol_table.elements.len() as PrimId - 1);
        let cell_ty = &symbol_table.elements.last().unwrap().ty;
        let cell = match cell_ty {
            donut_lang::load::Ty::Zero => FreeCell::zero(last_prim),
            donut_lang::load::Ty::Succ(s, t) => {
                FreeCell::prim(last_prim, s.clone(), t.clone()).unwrap()
            }
        };
        let mut f = LayoutSolver::new();
        let cell = f.from_free(cell);
        let sol = f.solve(&cell);
        let cell = sol.convert(&cell);
        // log::debug!("Cell: {}", cell);
        let cell = cell.render();
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
