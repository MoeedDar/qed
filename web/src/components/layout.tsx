import {
  ResizableHandle,
  ResizablePanel,
  ResizablePanelGroup,
} from "@/components/ui/resizable";

import type { ReactNode } from "react";

type LayoutProps = {
  editor: ReactNode;
  panel: ReactNode;
  showPanel: boolean;
  statusBar: ReactNode;
  sidebar: ReactNode;
};

export function Layout({ editor, panel, showPanel, statusBar, sidebar }: LayoutProps) {
  return (
    <div className="flex w-screen h-screen">
      {sidebar}
      <div className="flex flex-col flex-1 overflow-hidden">
        <ResizablePanelGroup orientation="vertical" className="grow">
          <ResizablePanel>{editor}</ResizablePanel>

          {showPanel && (
            <>
              <ResizableHandle withHandle />
              <ResizablePanel defaultSize={256}>{panel}</ResizablePanel>
            </>
          )}
        </ResizablePanelGroup>

        {statusBar}
      </div>
    </div>
  );
}
