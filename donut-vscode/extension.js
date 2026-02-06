"use strict";
const vscode = require("vscode");
const lc = require("vscode-languageclient");

let client;

function log(s) {
  vscode.window.showInformationMessage(s);
}

function logError(s) {
  vscode.window.showErrorMessage(s);
}

async function start() {
  if (client) {
    logError("donut-vscode already started");
    return;
  }
  const serverOptions = {
    command: "donut-ls"
  };
  const clientOptions = {
    documentSelector: [{ scheme: "file", language: "donut" }]
  };
  try {
    client = new lc.LanguageClient("donut", "donut-ls-client", serverOptions, clientOptions);
    await client.start();
    log("donut-vscode started");
  } catch(e) {
    logError(`Failed to start donut-ls: ${e}`);
  }
}

async function restart() {
  if(!client) {
    logError("donut-vscode not started yet");
    return;
  }
  try {
    await client.restart();
    log("donut-vscode restarted");
  } catch(e) {
    logError(`Failed to restart donut-ls: ${e}`);
  }
}

async function stop() {
  if(!client) {
    logError("donut-vscode already stopped");
    return;
  }
  try {
    await client.stop();
    log("donut-vscode stopped");
    client = undefined;
  } catch(e) {
    logError(`Failed to stop donut-ls: ${e}`);
  }
}

async function activate(context) {
  log("donut-vscode activated");
  context.subscriptions.push(
    vscode.commands.registerCommand("donut-vscode.restartServer", () => restart())
  );
  context.subscriptions.push(
    vscode.commands.registerCommand("donut-vscode.startServer", () => start())
  );
  context.subscriptions.push(
    vscode.commands.registerCommand("donut-vscode.stopServer", () => stop())
  );
  await start();
}

function deactivate() {
  if (client) {
    return client.stop();
  }
}

module.exports = {
  activate,
  deactivate
};