const features = {
  lsp: require('./features/lsp.js'),
  semanticHighlighting: require('./features/semantic-highlighting.js')
}


/** Runs with an ".elm" file is opened, or if an "elm.json" file is detected in the root of the project.
 * 
 * (See "activationEvents" in the `package.json`)
 * 
 * @param {import('vscode').ExtensionContext} context 
 * @returns {void}
 */
async function activate(context) {
  console.log("Activate!")
  features.lsp.activate(context)
  features.semanticHighlighting.activate(context)
}


/** Called when the plugin is deactivated (editor is closed, etc)
 * @returns {void}
 */
function deactivate() {
  // Do nothing!
  features.lsp.deactivate()
}


module.exports = { activate, deactivate }