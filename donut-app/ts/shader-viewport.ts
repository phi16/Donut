import type { WasmShaderView } from "./wasm-api";

export interface Viewport {
  // Center of the viewport in UV space
  cx: number;
  cy: number;
  // Half-size of the viewport (before aspect correction)
  half: number;
}

export class ShaderViewport {
  private shaderView: WasmShaderView;
  private wrapEl: HTMLElement;
  private canvasEl: HTMLCanvasElement;
  private infoEl: HTMLElement;
  private viewportEl: HTMLElement;
  private handleEl: HTMLElement;
  private outputTopEl: HTMLElement;

  private vp: Viewport = { cx: 0.5, cy: 0.5, half: 0.5 };
  private dragging = false;
  private dragStartX = 0;
  private dragStartY = 0;
  private dragStartVp: Viewport = { cx: 0.5, cy: 0.5, half: 0.5 };

  private hasShader = false;

  constructor(shaderView: WasmShaderView) {
    this.shaderView = shaderView;
    this.viewportEl = document.getElementById("shader-viewport")!;
    this.handleEl = document.getElementById("handle-output-v")!;
    this.outputTopEl = document.getElementById("output-top")!;
    this.wrapEl = document.getElementById("shader-canvas-wrap")!;
    this.canvasEl = document.getElementById("shader-canvas") as HTMLCanvasElement;
    this.infoEl = document.getElementById("shader-viewport-info")!;

    this.setupEvents();
    this.updateInfo();
  }

  /** Compute the actual viewport rect, corrected for canvas aspect ratio. */
  private viewportRect(): { minX: number; minY: number; maxX: number; maxY: number } {
    const w = this.canvasEl.width || 1;
    const h = this.canvasEl.height || 1;
    const aspect = w / h;

    let halfX: number, halfY: number;
    if (aspect >= 1) {
      halfX = this.vp.half * aspect;
      halfY = this.vp.half;
    } else {
      halfX = this.vp.half;
      halfY = this.vp.half / aspect;
    }

    return {
      minX: this.vp.cx - halfX,
      minY: this.vp.cy - halfY,
      maxX: this.vp.cx + halfX,
      maxY: this.vp.cy + halfY,
    };
  }

  private setupEvents() {
    // Pan: mouse drag
    this.wrapEl.addEventListener("mousedown", (e) => {
      if (e.button !== 0) return;
      e.preventDefault();
      this.dragging = true;
      this.dragStartX = e.clientX;
      this.dragStartY = e.clientY;
      this.dragStartVp = { ...this.vp };
      this.wrapEl.classList.add("dragging");
    });

    window.addEventListener("mousemove", (e) => {
      if (!this.dragging) return;
      const rect = this.wrapEl.getBoundingClientRect();
      const vr = this.viewportRect();
      const dx = (e.clientX - this.dragStartX) / rect.width;
      const dy = (e.clientY - this.dragStartY) / rect.height;
      const rangeX = vr.maxX - vr.minX;
      const rangeY = vr.maxY - vr.minY;
      this.vp.cx = this.dragStartVp.cx - dx * rangeX;
      this.vp.cy = this.dragStartVp.cy + dy * rangeY; // Y flipped
      this.renderNow();
    });

    window.addEventListener("mouseup", () => {
      if (!this.dragging) return;
      this.dragging = false;
      this.wrapEl.classList.remove("dragging");
    });

    // Zoom: wheel
    this.wrapEl.addEventListener("wheel", (e) => {
      e.preventDefault();
      const factor = e.deltaY > 0 ? 1.1 : 1 / 1.1;
      const rect = this.wrapEl.getBoundingClientRect();
      const mx = (e.clientX - rect.left) / rect.width;
      const my = 1 - (e.clientY - rect.top) / rect.height;

      const vr = this.viewportRect();
      const uvX = vr.minX + mx * (vr.maxX - vr.minX);
      const uvY = vr.minY + my * (vr.maxY - vr.minY);

      // Zoom centered on mouse: shift center toward mouse, scale half
      this.vp.cx = uvX + (this.vp.cx - uvX) * factor;
      this.vp.cy = uvY + (this.vp.cy - uvY) * factor;
      this.vp.half *= factor;

      this.renderNow();
    }, { passive: false });

    // Reset button
    document.getElementById("shader-reset")!.addEventListener("click", () => {
      this.vp = { cx: 0.5, cy: 0.5, half: 0.5 };
      this.renderNow();
    });

    // Vertical resize handle between output-top and shader-viewport
    {
      let vDragging = false;
      let startY = 0;
      let startTopH = 0;
      let startBotH = 0;

      this.handleEl.addEventListener("mousedown", (e) => {
        e.preventDefault();
        vDragging = true;
        startY = e.clientY;
        startTopH = this.outputTopEl.getBoundingClientRect().height;
        startBotH = this.viewportEl.getBoundingClientRect().height;
        this.handleEl.classList.add("active");
        document.body.style.cursor = "row-resize";
        document.body.style.userSelect = "none";
      });

      window.addEventListener("mousemove", (e) => {
        if (!vDragging) return;
        const dy = e.clientY - startY;
        const newTop = Math.max(80, startTopH + dy);
        const newBot = Math.max(100, startBotH - dy);
        if (newTop >= 80 && newBot >= 100) {
          this.outputTopEl.style.flex = "none";
          this.outputTopEl.style.height = newTop + "px";
          this.viewportEl.style.flex = "none";
          this.viewportEl.style.height = newBot + "px";
        }
      });

      window.addEventListener("mouseup", () => {
        if (!vDragging) return;
        vDragging = false;
        this.handleEl.classList.remove("active");
        document.body.style.cursor = "";
        document.body.style.userSelect = "";
      });
    }

    // Resize canvas to fill wrapper — render immediately to avoid flicker
    const ro = new ResizeObserver(() => {
      this.syncCanvasSize();
      this.renderNow();
    });
    ro.observe(this.wrapEl);
  }

  private syncCanvasSize() {
    const rect = this.wrapEl.getBoundingClientRect();
    const w = Math.floor(rect.width);
    const h = Math.floor(rect.height);
    if (w > 0 && h > 0 && (this.canvasEl.width !== w || this.canvasEl.height !== h)) {
      this.canvasEl.width = w;
      this.canvasEl.height = h;
    }
  }

  private renderNow() {
    if (this.hasShader) {
      const vr = this.viewportRect();
      this.shaderView.render(vr.minX, vr.minY, vr.maxX, vr.maxY);
    }
    this.updateInfo();
  }

  private updateInfo() {
    const vr = this.viewportRect();
    const fmt = (n: number) => n.toFixed(3);
    this.infoEl.textContent =
      `(${fmt(vr.minX)}, ${fmt(vr.minY)}) - (${fmt(vr.maxX)}, ${fmt(vr.maxY)})`;
  }

  setShader(cellFunction: string, colorWrapper: string) {
    const source = [
      "#extension GL_OES_standard_derivatives : enable",
      "precision mediump float;",
      "uniform vec2 u_resolution;",
      "uniform vec2 u_viewport_min;",
      "uniform vec2 u_viewport_max;",
      "",
      cellFunction,
      "",
      colorWrapper,
      "",
      "float render(float d) {",
      "    float dd = abs(dFdx(d)) + abs(dFdy(d));",
      "    return clamp(- d / dd + 0.5, 0.0, 1.0);",
      "}",
      "",
      "void main() {",
      "    vec2 uv = u_viewport_min + (gl_FragCoord.xy / u_resolution) * (u_viewport_max - u_viewport_min);",
      "    vec3 color = cell_color(uv);",
      "    vec2 q = abs(uv - vec2(0.5)) - vec2(0.5);",
      "    float d = length(max(q, 0.0)) + min(max(q.x, q.y), 0.0);",
      "    vec3 border = mix(color * color, vec3(0.2), 0.5);",
      "    vec3 outer = 0.8 * color;",
      "    color = mix(mix(outer, border, render(d - 0.01)), color, render(d));",
      "    gl_FragColor = vec4(color, 1.0);",
      "}",
    ].join("\n");
    this.shaderView.set_shader(source);
    this.hasShader = true;
    this.syncCanvasSize();
    this.renderNow();
  }

  show() {
    this.viewportEl.classList.add("visible");
    this.handleEl.classList.add("visible");
    this.syncCanvasSize();
    this.renderNow();
  }

  hide() {
    this.viewportEl.classList.remove("visible");
    this.handleEl.classList.remove("visible");
    this.hasShader = false;
  }
}
