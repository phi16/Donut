use wasm_bindgen::JsCast;
use web_sys::{WebGlProgram, WebGlRenderingContext, WebGlShader};

const VERTEX_SHADER: &str = "\
attribute vec2 a_position;
void main() {
    gl_Position = vec4(a_position, 0.0, 1.0);
}";

pub struct ShaderView {
    canvas: web_sys::HtmlCanvasElement,
    gl: WebGlRenderingContext,
    program: Option<WebGlProgram>,
}

impl ShaderView {
    pub fn new(canvas: web_sys::HtmlCanvasElement) -> Option<Self> {
        let gl = canvas
            .get_context("webgl")
            .ok()??
            .dyn_into::<WebGlRenderingContext>()
            .ok()?;

        // Set up fullscreen quad vertex buffer
        let vertices: [f32; 12] = [
            -1.0, -1.0, 1.0, -1.0, -1.0, 1.0, -1.0, 1.0, 1.0, -1.0, 1.0, 1.0,
        ];
        let buffer = gl.create_buffer()?;
        gl.bind_buffer(WebGlRenderingContext::ARRAY_BUFFER, Some(&buffer));
        unsafe {
            let array = js_sys::Float32Array::view(&vertices);
            gl.buffer_data_with_array_buffer_view(
                WebGlRenderingContext::ARRAY_BUFFER,
                &array,
                WebGlRenderingContext::STATIC_DRAW,
            );
        }

        Some(ShaderView {
            canvas,
            gl,
            program: None,
        })
    }

    pub fn set_shader(&mut self, fragment_source: &str) -> Result<(), String> {
        // Clean up old program
        if let Some(old) = self.program.take() {
            self.gl.delete_program(Some(&old));
        }

        let vert = compile_shader(&self.gl, WebGlRenderingContext::VERTEX_SHADER, VERTEX_SHADER)?;
        let frag = compile_shader(
            &self.gl,
            WebGlRenderingContext::FRAGMENT_SHADER,
            fragment_source,
        )?;

        let program = self.gl.create_program().ok_or("failed to create program")?;
        self.gl.attach_shader(&program, &vert);
        self.gl.attach_shader(&program, &frag);
        self.gl.link_program(&program);

        if !self
            .gl
            .get_program_parameter(&program, WebGlRenderingContext::LINK_STATUS)
            .as_bool()
            .unwrap_or(false)
        {
            let log = self
                .gl
                .get_program_info_log(&program)
                .unwrap_or_default();
            self.gl.delete_program(Some(&program));
            return Err(format!("link error: {}", log));
        }

        self.gl.delete_shader(Some(&vert));
        self.gl.delete_shader(Some(&frag));
        self.program = Some(program);
        Ok(())
    }

    pub fn render(&self) {
        let Some(ref program) = self.program else {
            return;
        };

        let w = self.canvas.width();
        let h = self.canvas.height();
        self.gl.viewport(0, 0, w as i32, h as i32);
        self.gl.clear_color(0.0, 0.0, 0.0, 1.0);
        self.gl.clear(WebGlRenderingContext::COLOR_BUFFER_BIT);

        self.gl.use_program(Some(program));

        // Set u_resolution
        let loc = self.gl.get_uniform_location(program, "u_resolution");
        self.gl
            .uniform2f(loc.as_ref(), w as f32, h as f32);

        // Bind vertex attribute
        let pos_loc = self.gl.get_attrib_location(program, "a_position") as u32;
        self.gl.enable_vertex_attrib_array(pos_loc);
        self.gl.vertex_attrib_pointer_with_i32(
            pos_loc,
            2,
            WebGlRenderingContext::FLOAT,
            false,
            0,
            0,
        );

        self.gl.draw_arrays(WebGlRenderingContext::TRIANGLES, 0, 6);
    }

    pub fn show(&self) {
        let _ = self.canvas.class_list().add_1("visible");
    }

    pub fn hide(&self) {
        let _ = self.canvas.class_list().remove_1("visible");
    }
}

fn compile_shader(
    gl: &WebGlRenderingContext,
    shader_type: u32,
    source: &str,
) -> Result<WebGlShader, String> {
    let shader = gl.create_shader(shader_type).ok_or("failed to create shader")?;
    gl.shader_source(&shader, source);
    gl.compile_shader(&shader);

    if !gl
        .get_shader_parameter(&shader, WebGlRenderingContext::COMPILE_STATUS)
        .as_bool()
        .unwrap_or(false)
    {
        let log = gl.get_shader_info_log(&shader).unwrap_or_default();
        gl.delete_shader(Some(&shader));
        return Err(format!("compile error: {}", log));
    }

    Ok(shader)
}
