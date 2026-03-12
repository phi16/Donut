import { createEditor, EditorHandle } from "./editor/editor";
import type { WasmModule, WasmEngine, EntryDesc, AnalysisResult } from "./wasm-api";

export class App {
  private wasm: WasmModule;
  private engine: WasmEngine;
  private editor: EditorHandle;
  private canvas: HTMLCanvasElement;
  private context: CanvasRenderingContext2D;
  private entrySelect: HTMLSelectElement;
  private evalResultEl: HTMLElement;
  private diagnosticsEl: HTMLElement;
  private shaderView: any; // WasmShaderView
  private canvasStep: (
    engine: WasmEngine,
    context: CanvasRenderingContext2D,
    width: number,
    height: number,
    mouseX: number,
    mouseY: number,
    pressing: boolean
  ) => void;

  private mouseX = 0;
  private mouseY = 0;
  private pressing = false;
  private showGlsl = false;
  private showShader = false;

  private debounceTimer: number | null = null;
  private readonly DEBOUNCE_MS = 300;

  constructor(wasm: WasmModule & Record<string, any>) {
    this.wasm = wasm;
    this.canvasStep = wasm.canvas_step;

    // Get DOM elements
    this.canvas = document.getElementById("canvas") as HTMLCanvasElement;
    this.context = this.canvas.getContext("2d")!;
    this.entrySelect = document.getElementById(
      "entry-select"
    ) as HTMLSelectElement;
    this.evalResultEl = document.getElementById("eval-result") as HTMLElement;
    this.diagnosticsEl = document.getElementById("diagnostics") as HTMLElement;

    // Shader view
    const shaderCanvas = document.getElementById(
      "shader-canvas"
    ) as HTMLCanvasElement | null;
    if (shaderCanvas) {
      shaderCanvas.width = 380;
      shaderCanvas.height = 380;
      this.shaderView = wasm.create_shader_view(shaderCanvas) ?? null;
    }

    // Initialize engine with default code
    const defaultCode: string = wasm.default_code();
    this.engine = new wasm.WasmEngine(defaultCode);

    // Create CodeMirror editor
    const editorContainer = document.getElementById("code-editor-cm")!;
    this.editor = createEditor(editorContainer, defaultCode, (code) => {
      this.onCodeChange(code);
    });

    // Set up event listeners
    this.setupEvents();

    // Initial UI update
    this.populateSelect();
    this.updateEvalResult();

    // Initial analysis
    const analysis: AnalysisResult = wasm.analyze(defaultCode);
    this.editor.applyAnalysis(analysis);

    // Start animation loop
    this.animate();
  }

  private setupEvents() {
    // Resize
    const resize = () => {
      this.canvas.width = window.innerWidth;
      this.canvas.height = window.innerHeight;
    };
    resize();
    window.addEventListener("resize", resize);

    // Mouse tracking
    window.addEventListener("mousemove", (e) => {
      this.mouseX = e.clientX;
      this.mouseY = e.clientY;
    });
    this.canvas.addEventListener("mousedown", () => {
      this.pressing = true;
    });
    window.addEventListener("mouseup", () => {
      this.pressing = false;
    });

    // Entry select
    this.entrySelect.addEventListener("change", () => {
      const index = parseInt(this.entrySelect.value, 10);
      if (!isNaN(index)) {
        this.engine.select_entry(index);
        this.updateEvalResult();
      }
    });

    // GLSL toggle
    const glslToggle = document.getElementById("glsl-toggle") as HTMLInputElement | null;
    if (glslToggle) {
      glslToggle.addEventListener("change", () => {
        this.showGlsl = glslToggle.checked;
        this.updateEvalResult();
      });
    }

    // Shader toggle
    const shaderToggle = document.getElementById("shader-toggle") as HTMLInputElement | null;
    if (shaderToggle) {
      shaderToggle.addEventListener("change", () => {
        this.showShader = shaderToggle.checked;
        this.updateEvalResult();
      });
    }
  }

  private onCodeChange(code: string) {
    if (this.debounceTimer !== null) {
      clearTimeout(this.debounceTimer);
    }
    this.debounceTimer = window.setTimeout(() => {
      this.debounceTimer = null;
      this.engine.update_code(code);
      this.populateSelect();
      this.updateEvalResult();

      // Run analysis and apply editor intelligence
      const analysis: AnalysisResult = this.wasm.analyze(code);
      this.editor.applyAnalysis(analysis);
    }, this.DEBOUNCE_MS);
  }

  private populateSelect() {
    this.entrySelect.innerHTML = "";
    const entries: EntryDesc[] = this.engine.root_entries();
    const selectedIndex = this.engine.selected_index();
    for (const entry of entries) {
      const option = document.createElement("option");
      option.textContent = `${entry.name} (${entry.dimension}d)`;
      option.value = entry.index.toString();
      const [r, g, b] = entry.color;
      option.style.color = `rgb(${r}, ${g}, ${b})`;
      if (entry.index === selectedIndex) {
        option.selected = true;
      }
      this.entrySelect.appendChild(option);
    }
  }

  private updateEvalResult() {
    // Diagnostics
    const diags: string[] = this.engine.diagnostics();
    if (diags.length === 0) {
      this.diagnosticsEl.classList.remove("has-errors");
      this.diagnosticsEl.textContent = "";
    } else {
      this.diagnosticsEl.classList.add("has-errors");
      this.diagnosticsEl.textContent = diags.join("\n");
    }

    // Eval result
    const evalText = this.engine.eval_result();
    if (!evalText) {
      this.evalResultEl.textContent = "";
      this.evalResultEl.classList.remove("evaluable");
      this.hideShader();
      return;
    }

    if (this.engine.is_evaluable()) {
      this.evalResultEl.classList.add("evaluable");
    } else {
      this.evalResultEl.classList.remove("evaluable");
    }

    let text = evalText;
    let shaderShown = false;

    if (this.showGlsl || this.showShader) {
      const glsl = this.engine.compile_glsl();
      if (glsl && this.showGlsl) {
        text += "\n\n--- GLSL ---\n" + glsl;
      }

      if (this.showShader && this.shaderView) {
        const fragSrc = this.engine.compile_fragment_shader();
        if (fragSrc) {
          try {
            this.shaderView.set_shader(fragSrc);
            this.shaderView.render();
            this.shaderView.show();
            shaderShown = true;
          } catch (e: any) {
            text += `\nshader error: ${e}`;
          }
        }
      }
    }

    if (!shaderShown) {
      this.hideShader();
    }

    this.evalResultEl.textContent = text;
  }

  private hideShader() {
    if (this.shaderView) {
      this.shaderView.hide();
    }
  }

  private animate() {
    const loop_ = () => {
      this.canvasStep(
        this.engine,
        this.context,
        this.canvas.width,
        this.canvas.height,
        this.mouseX,
        this.mouseY,
        this.pressing
      );
      requestAnimationFrame(loop_);
    };
    requestAnimationFrame(loop_);
  }
}
