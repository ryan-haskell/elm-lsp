const vscode = require('vscode')
const { exec } = require("child_process")
const { LanguageClient, TransportKind, CloseAction, ErrorAction } = require("vscode-languageclient/node")

/** @type {LanguageClient} */
let client = null

/**
 * @param {import("vscode").ExtensionContext} context 
 * @returns {Promise<void>}
 */
async function activate(context) {
  let useGoLsp = false

  let command, isElmLspInstalled

  if (useGoLsp) {
    command = '/Users/ryan/code/tjdevries/educationalsp/main'
    isElmLspInstalled = true
  } else {
    command = '/Users/ryan/code/ryan-haskell/elm-lsp/server/dist-newstyle/build/x86_64-osx/ghc-9.4.8/elm-lsp-0.1.0.0/x/elm-lsp/build/elm-lsp/elm-lsp'
  
    isElmLspInstalled = await new Promise((resolve) => {
      try {
        exec(`${command} --version`, (err) => err ? resolve(false) : resolve(true))
      } catch (_) {
        resolve(false)
      }
    })
  }

  if (isElmLspInstalled) {
    try {
      console.log("LSP is installed!")
      client = new LanguageClient(
        'elmLsp',
        'Elm Language Server',
        {
          command: command,
          transport: TransportKind.stdio,
          options: { encoding: 'utf8' }
        },
        {
          // traceOutputChannel: vscode.window.createOutputChannel('Elm LSP', 'elm'),
          documentSelector: [
            { scheme: 'file', language: 'elm' }
          ],
          errorHandler: {
            closed() {
              console.log("LSP closed")
              return {
                action: CloseAction.DoNotRestart,
                handled: true
              }
            },
            error(error, message, count) {
              console.log("LSP error")
              return {
                action: ErrorAction.Shutdown,
                handled: true
              }
            },
          }
        }
      )
  
      console.log("Starting LSP client...")
      client.start()
    } catch (err) {
      console.error('Elm LSP failed to start', err)
    }
  } else {
    console.error('elm-lsp is not installed')
  }
}

async function deactivate() {
  if (client) {
    console.log("Deactivating LSP client!")
    client.stop()
  }
}

module.exports = { activate, deactivate }