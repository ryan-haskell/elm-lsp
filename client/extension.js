const vscode = require('vscode')

/**
 * @param {import('vscode').ExtensionContext} context 
 * @returns {void}
 */
function activate(context) {
  console.log("Activated")
}

/**
 * @returns {void}
 */
function deactivate() {
  console.log("Deactivated")
}

// Expose these functions to the VS Code editor
module.exports = {
  activate,
  deactivate
}