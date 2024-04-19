const vscode = require('vscode')

/**
 * @param {import("vscode").ExtensionContext} context 
 * @returns {Promise<void>}
 */
async function activate(context) {
  const legend = new vscode.SemanticTokensLegend(
    ["module", "keyword"],
    ["declaration"]
  )

  /**
   * @type {import("vscode").DocumentSemanticTokensProvider}
   */
  let provider = {
    provideDocumentSemanticTokens(document) {
      const tokens = new vscode.SemanticTokensBuilder(legend)

      tokens.push(
        new vscode.Range(
          new vscode.Position(0, 0),
          new vscode.Position(0, 6),
        ),
        'keyword',
        []
      )

      tokens.push(
        new vscode.Range(
          new vscode.Position(0, 7),
          new vscode.Position(0, 11),
        ),
        'module',
        ['declaration']
      )

      return tokens.build()
    }
  }

  vscode.languages.registerDocumentSemanticTokensProvider(
    { language: 'elm', scheme: 'file' },
    provider,
    legend
  )
}

module.exports = { activate }