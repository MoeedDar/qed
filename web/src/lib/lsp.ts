import type { Transport } from '@codemirror/lsp-client';

type Handler = (value: string) => void;

export type JsonRpcRequest = {
  jsonrpc: '2.0';
  id?: number;
  method: string;
  params?: any;
};

export type Diagnostic = {
  severity: number;
  range: {
    start: { line: number; character: number };
    end: { line: number; character: number };
  };
  message: string;
};

export type ServerCapabilities = {
  textDocumentSync: number;
  hoverProvider: boolean;
  completionProvider: { triggerCharacters: string[] };
};

export type LanguageServer = {
  capabilities: ServerCapabilities;
  onOpen(uri: string, text: string): Diagnostic[];
  onChange(uri: string, text: string): Diagnostic[];
  onHover(text: string, line: number, character: number): unknown;
  completions: { label: string; kind: number }[];
};

export class JsonRpcConnection implements Transport {
  private handlers: Handler[] = [];

  send(message: string) {
    this.dispatch(JSON.parse(message));
  }

  subscribe(handler: Handler) {
    this.handlers.push(handler);
  }

  unsubscribe(handler: Handler) {
    this.handlers = this.handlers.filter((h) => h !== handler);
  }

  private dispatch(msg: JsonRpcRequest) {
    switch (msg.method) {
      case 'initialized':
      case 'shutdown':
      case 'exit':
        break;
      default:
        this.handle(msg);
    }
  }

  protected handle(msg: JsonRpcRequest) {
    if (msg.id !== undefined) {
      this.error(msg.id, -32601, `method not found: ${msg.method}`);
    }
  }

  reply(id: number | undefined, result: unknown) {
    if (id !== undefined) {
      this.emit({ jsonrpc: '2.0', id, result });
    }
  }

  notify(method: string, params: unknown) {
    this.emit({ jsonrpc: '2.0', method, params });
  }

  error(id: number, code: number, message: string) {
    this.emit({ jsonrpc: '2.0', id, error: { code, message } });
  }

  emit(msg: object) {
    const raw = JSON.stringify(msg);
    for (const h of this.handlers) {
      h(raw);
    }
  }
}

export class LanguageServerConnection extends JsonRpcConnection {
  private docs = new Map<string, string>();

  constructor(private server: LanguageServer) {
    super();
  }

  protected handle(msg: JsonRpcRequest) {
    switch (msg.method) {
      case 'initialize':
        this.reply(msg.id, {
          capabilities: this.server.capabilities,
          serverInfo: { name: 'lsp', version: '0.0.1' },
        });
        break;

      case 'textDocument/didOpen':
        this.open(msg.params);
        break;

      case 'textDocument/didChange':
        this.change(msg.params);
        break;

      case 'textDocument/hover': {
        const { line, character } = msg.params.position;
        this.reply(msg.id, this.server.onHover(this.text(msg.params.textDocument.uri), line, character));
        break;
      }

      case 'textDocument/completion':
        this.reply(msg.id, this.server.completions);
        break;

      default:
        super.handle(msg);
    }
  }

  private open(params: any) {
    const { uri, text } = params.textDocument;
    this.docs.set(uri, text);
    this.publish(uri, this.server.onOpen(uri, text));
  }

  private change(params: any) {
    const { uri } = params.textDocument;
    const text = params.contentChanges.at(-1).text;
    this.docs.set(uri, text);
    this.publish(uri, this.server.onChange(uri, text));
  }

  private text(uri: string) {
    return this.docs.get(uri) ?? '';
  }

  private publish(uri: string, diagnostics: Diagnostic[]) {
    this.notify('textDocument/publishDiagnostics', { uri, diagnostics });
  }

  diagnostics(uri: string): Diagnostic[] {
    return this.server.onOpen(uri, this.text(uri));
  }
}
