import { Panel, PanelRow } from '@/components/panel';

import type { Diagnostic } from '@/lib/lsp';

import { TriangleAlert } from 'lucide-react';

type ProblemsProps = {
  problems: Diagnostic[];
  onJump: (line: number, character: number) => void;
};

export function Problems({ problems, onJump }: ProblemsProps) {
  return (
    <Panel
      icon={TriangleAlert}
      title="Problems"
      count={problems.length}
    >
      {problems.map((problem) => {
        const { start, end } = problem.range;
        return (
          <PanelRow
            key={`${start.line}:${start.character}:${problem.message}`}
            onSelect={() => onJump(start.line, start.character)}
          >
            <div className="w-full text-start">{problem.message}</div>
            <div className="text-muted-foreground">
              {start.line + 1}:{start.character + 1} - {end.line + 1}:
              {end.character + 1}
            </div>
          </PanelRow>
        );
      })}
    </Panel>
  );
}
