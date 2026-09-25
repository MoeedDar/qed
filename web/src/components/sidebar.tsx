import { Link } from '@tanstack/react-router';

import {
  Collapsible,
  CollapsibleContent,
  CollapsibleTrigger,
} from '@/components/ui/collapsible';
import {
  Sidebar,
  SidebarContent,
  SidebarGroup,
  SidebarGroupContent,
  SidebarMenu,
  SidebarMenuButton,
  SidebarMenuItem,
  SidebarMenuSub,
  SidebarMenuSubButton,
  SidebarMenuSubItem,
} from '@/components/ui/sidebar';
import { byTopic, slug } from '@/lib/puzzles';

import type { LucideIcon } from 'lucide-react';

import { ChevronRight, Puzzle, SquareTerminalIcon } from 'lucide-react';

type SidebarItem = {
  label: string;
  to?: string;
  params?: { puzzle: string };
  icon?: LucideIcon;
  items?: SidebarItem[];
};

function capitalise(text: string) {
  return text.charAt(0).toUpperCase() + text.slice(1);
}

const collapsiblePanelClass =
  'h-(--collapsible-panel-height) overflow-hidden transition-[height,opacity] duration-200 ease-out data-[starting-style]:h-0 data-[starting-style]:opacity-0 data-[ending-style]:h-0 data-[ending-style]:opacity-0';

function items(): SidebarItem[] {
  const levels = byTopic().map((group) => ({
    label: capitalise(group.topic),
    items: group.puzzles.map((puzzle) => ({
      label: puzzle.title,
      to: '/$puzzle',
      params: { puzzle: slug(puzzle) },
    })),
  }));

  return [
    { label: 'Playground', to: '/', icon: SquareTerminalIcon },
    { label: 'Levels', icon: Puzzle, items: levels },
  ];
}

export function AppSidebar() {
  return (
    <Sidebar>
      <SidebarContent>
        <SidebarGroup>
          <SidebarGroupContent>
            <SidebarItems items={items()} />
          </SidebarGroupContent>
        </SidebarGroup>
      </SidebarContent>
    </Sidebar>
  );
}

function SidebarSubItems({ items }: { items: SidebarItem[] }) {
  return (
    <SidebarMenuSub>
      {items.map((item) => {
        if (item.items && item.items.length > 0) {
          return (
            <Collapsible key={item.label} className="group/sub-collapsible">
              <SidebarMenuSubItem>
                <CollapsibleTrigger>
                  <SidebarMenuSubButton>
                    <span>{item.label}</span>
                    <ChevronRight className="ml-auto h-4 w-4 transition-transform duration-200 group-data-open/sub-collapsible:rotate-90" />
                  </SidebarMenuSubButton>
                </CollapsibleTrigger>
                <CollapsibleContent className={collapsiblePanelClass}>
                  <SidebarSubItems items={item.items} />
                </CollapsibleContent>
              </SidebarMenuSubItem>
            </Collapsible>
          );
        }

        return (
          <SidebarMenuSubItem key={item.label}>
            <SidebarMenuSubButton
              render={
                item.to ? (
                  <Link to={item.to} params={item.params}>
                    {item.label}
                  </Link>
                ) : undefined
              }
            >
              {item.to ? null : item.label}
            </SidebarMenuSubButton>
          </SidebarMenuSubItem>
        );
      })}
    </SidebarMenuSub>
  );
}

function SidebarItems({ items }: { items: SidebarItem[] }) {
  return (
    <SidebarMenu className="w-full">
      {items.map((item) => {
        const Icon = item.icon;

        if (item.items && item.items.length > 0) {
          return (
            <Collapsible key={item.label} className="group/collapsible">
              <SidebarMenuItem>
                <CollapsibleTrigger>
                  <SidebarMenuButton>
                    {Icon && <Icon />}
                    <span>{item.label}</span>
                    <ChevronRight className="ml-auto size-4 transition-transform duration-200 group-data-open/collapsible:rotate-90" />
                  </SidebarMenuButton>
                </CollapsibleTrigger>
                <CollapsibleContent className={collapsiblePanelClass}>
                  <SidebarSubItems items={item.items} />
                </CollapsibleContent>
              </SidebarMenuItem>
            </Collapsible>
          );
        }

        return (
          <SidebarMenuItem key={item.label}>
            <SidebarMenuButton
              render={
                item.to ? (
                  <Link to={item.to} params={item.params}>
                    {item.label}
                  </Link>
                ) : undefined
              }
            >
              {item.to ? null : item.label}
            </SidebarMenuButton>
          </SidebarMenuItem>
        );
      })}
    </SidebarMenu>
  );
}
