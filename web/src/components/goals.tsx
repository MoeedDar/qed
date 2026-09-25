import { Panel, PanelRow } from '@/components/panel';

import type { QedGoal } from '@/lib/qed-lsp';

import { Target } from 'lucide-react';

type GoalsProps = {
  goals: QedGoal[];
  current: number;
  onSelect: (goal: QedGoal) => void;
};

function GoalRow({
  goal,
  active,
  onSelect,
}: {
  goal: QedGoal;
  active: boolean;
  onSelect: () => void;
}) {
  return (
    <PanelRow active={active} onSelect={onSelect}>
      <div className="w-full text-start flex flex-row gap-2">
        {goal.context.map((hypothesis) => (
          <div
            key={`${hypothesis.name}:${hypothesis.typ}`}
            className="text-muted-foreground"
          >
            {hypothesis.name} : {hypothesis.typ}
          </div>
        ))}

        <div>
          <span className="select-none text-muted-foreground">⊢ </span>
          {goal.goal}
        </div>
      </div>

      <div className="text-muted-foreground">
        {goal.start.line + 1}:{goal.start.character + 1} - {goal.end.line + 1}:
        {goal.end.character + 1}
      </div>
    </PanelRow>
  );
}

export function Goals({ goals, current, onSelect }: GoalsProps) {
  return (
    <Panel icon={Target} title="Goals" count={goals.length}>
      {goals.map((goal, index) => (
        <GoalRow
          key={`${goal.start.line}:${goal.start.character}`}
          goal={goal}
          active={current === index || (current < 0 && index === 0)}
          onSelect={() => onSelect(goal)}
        />
      ))}
    </Panel>
  );
}
