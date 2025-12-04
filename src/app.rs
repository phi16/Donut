use donut_core::cell::*;
use donut_core::table::*;
use donut_renderer::geometry_old::extract_geometry;
use donut_renderer::geometry_old::Geometry;
use donut_renderer::render::RGeometry;
use donut_renderer::render::Renderer;
use donut_util::println;
use nonempty::nonempty;
use std::rc::Rc;

pub struct App {
    renderer: Renderer,
    canvas: web_sys::HtmlCanvasElement,
    context: web_sys::CanvasRenderingContext2d,
    prim_table: Rc<PrimTable>,
    cell: Rc<LayoutCell>,
    geometry: RGeometry,
    t: f32,
}

impl App {
    pub fn new(
        canvas: web_sys::HtmlCanvasElement,
        context: web_sys::CanvasRenderingContext2d,
    ) -> Self {
        let prim_table = Rc::new(PrimTable::default());
        let i = prim_table.prim("i");
        let ii = prim_table.id(&i, 50);
        let ij = prim_table.id(&i, 150);
        let m = prim_table.prim("m");
        let w = prim_table.prim("w");
        let g1 = prim_table.comp(nonempty![Rc::clone(&w), Rc::clone(&ij)], 0, 20);
        let g2 = prim_table.comp(nonempty![Rc::clone(&ii), Rc::clone(&m)], 0, 80);
        let cell = prim_table.comp(nonempty![g1, g2], 1, 40);

        let test_cell = cell; // prim_table.prim("w");
                              // println(&format!("Cell: {:#?}", test_cell));

        let assoc = prim_table.prim("assoc");
        let test_cell = assoc;

        let g = extract_geometry(&test_cell);
        let rg = RGeometry::from(&g);

        // println(&format!("Geometry: {:#?}", g));

        let cell = PaddedCell::from_cell(test_cell).s().cell;

        Self {
            renderer: Renderer::new(context.clone(), Rc::clone(&prim_table)),
            canvas,
            context,
            prim_table,
            cell,
            geometry: rg,
            t: 0.0,
        }
    }

    pub fn step(&mut self) {
        let width = self.canvas.width() as f64;
        let height = self.canvas.height() as f64;
        self.context.set_fill_style_str("rgb(40 40 40)");
        self.context.fill_rect(0.0, 0.0, width, height);
        self.renderer.push();
        self.renderer.offset(50, 50);
        self.renderer.render(&self.cell, 0, 1);
        self.renderer.offset(450, 0);
        let x = self.t.sin() * 0.5 + 0.5;
        self.renderer
            .render_geometry(&self.geometry.slice(x * self.geometry.size[2] as f32));

        self.renderer.pop();
        self.t += 0.03;
    }
}
