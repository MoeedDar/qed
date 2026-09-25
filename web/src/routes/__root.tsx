import { HeadContent, Scripts, createRootRoute } from "@tanstack/react-router";

import { SidebarProvider } from "@/components/ui/sidebar";
import { head } from "@/lib/head";

export const Route = createRootRoute({
  head,
  shellComponent: RootDocument,
  ssr: false,
});

function RootDocument({ children }: React.PropsWithChildren) {
  return (
    <html lang="en" className="dark">
      <head>
        <HeadContent />
      </head>
      <body>
        <SidebarProvider>
          {children}
          <Scripts />
        </SidebarProvider>
      </body>
    </html>
  );
}
