package net.flatmap.cobra;

import org.eclipse.lsp4j.MessageActionItem;
import org.eclipse.lsp4j.MessageParams;
import org.eclipse.lsp4j.PublishDiagnosticsParams;
import org.eclipse.lsp4j.ShowMessageRequestParams;
import org.eclipse.lsp4j.services.LanguageClient;

import java.util.concurrent.CompletableFuture;

/**
 * default implementation of a Language Client,
 * has No Ops for every interaction!
 *
 * can print request parameter to stdout
 */
public class NoOpClient implements LanguageClient {

    private final boolean printDebug;

    public NoOpClient() {
        this.printDebug = false;
    }

    public NoOpClient(boolean printDebug) {
        this.printDebug = printDebug;
    }

    @Override
    public void telemetryEvent(Object object) {
        if(printDebug) System.out.println("telemetry event "+object.toString());
    }

    @Override
    public void publishDiagnostics(PublishDiagnosticsParams diagnostics) {
        if(printDebug) System.out.println("publish diagnostics "+diagnostics.toString());
    }

    @Override
    public void showMessage(MessageParams messageParams) {
        if(printDebug) System.out.println("show message "+ messageParams.toString());
    }

    @Override
    public CompletableFuture<MessageActionItem> showMessageRequest(ShowMessageRequestParams requestParams) {
        if(printDebug) System.out.println("show message request "+requestParams.toString());
        return null;
    }

    @Override
    public void logMessage(MessageParams message) {
        if(printDebug) System.out.println("show log message "+message.toString());
    }
}
