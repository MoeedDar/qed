import { Button } from './ui/button';

import type { ElementType, ReactNode } from 'react';

import { cn } from 'cn';

type PanelProps = {
  icon: ElementType;
  title: string;
  count: number;
  children: ReactNode;
};

export function Panel({ icon, title, count, children }: PanelProps) {
  const Icon = icon;

  return (
    <section className="flex flex-col h-full overflow-hidden p-1">
      <div className="flex items-center gap-1 text-sm font-medium text-muted-foreground">
        <Icon className="size-3.5 mr-1" />
        {title}
        <span className="tabular-nums">({count})</span>
      </div>

      <div className="grow overflow-y-auto">{children}</div>
    </section>
  );
}

type PanelRowProps = {
  active?: boolean;
  onSelect?: () => void;
  children: ReactNode;
};

export function PanelRow({
  active = false,
  onSelect,
  children,
}: PanelRowProps) {
  return (
    <Button
      className={cn(
        'w-full justify-start rounded-lg p-1 select-text font-mono',
        active && 'bg-muted',
      )}
      variant="ghost"
      onClick={onSelect}
    >
      {children}
    </Button>
  );
}
