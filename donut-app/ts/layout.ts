export function setupResizeHandles() {
  setupHandle(
    "handle-left",
    "panel-canvas",
    "panel-output",
    "left"
  );
  setupHandle(
    "handle-right",
    "panel-output",
    "panel-editor",
    "left"
  );
}

function setupHandle(
  handleId: string,
  leftPanelId: string,
  rightPanelId: string,
  side: "left" | "right"
) {
  const handle = document.getElementById(handleId)!;
  const leftPanel = document.getElementById(leftPanelId)!;
  const rightPanel = document.getElementById(rightPanelId)!;

  let dragging = false;
  let startX = 0;
  let startLeftWidth = 0;
  let startRightWidth = 0;

  handle.addEventListener("mousedown", (e) => {
    e.preventDefault();
    dragging = true;
    startX = e.clientX;
    startLeftWidth = leftPanel.getBoundingClientRect().width;
    startRightWidth = rightPanel.getBoundingClientRect().width;
    handle.classList.add("active");
    document.body.style.cursor = "col-resize";
    document.body.style.userSelect = "none";
  });

  window.addEventListener("mousemove", (e) => {
    if (!dragging) return;
    const dx = e.clientX - startX;
    const newLeft = Math.max(100, startLeftWidth + dx);
    const newRight = Math.max(100, startRightWidth - dx);

    // Only apply if both panels stay above minimum
    if (newLeft >= 100 && newRight >= 100) {
      leftPanel.style.width = newLeft + "px";
      leftPanel.style.flex = "none";
      rightPanel.style.width = newRight + "px";
      rightPanel.style.flex = "none";
    }
  });

  window.addEventListener("mouseup", () => {
    if (!dragging) return;
    dragging = false;
    handle.classList.remove("active");
    document.body.style.cursor = "";
    document.body.style.userSelect = "";
  });
}
