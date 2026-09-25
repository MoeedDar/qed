import { useSidebar } from '@/components/ui/sidebar';
import { Toggle } from '@/components/ui/toggle';

import {
  Check,
  PanelLeftIcon,
  Target,
  TriangleAlert,
  XIcon,
} from 'lucide-react';

type StatusBarProps = {
  vim: boolean;
  onVimChange: (vim: boolean) => void;
  activePanel: 'goals' | 'problems' | null;
  onToggleGoals: (pressed: boolean) => void;
  onToggleProblems: (pressed: boolean) => void;
  goalCount: number;
  problemCount: number;
};

export function StatusBar({
  vim,
  onVimChange,
  activePanel,
  onToggleGoals,
  onToggleProblems,
  goalCount,
  problemCount,
}: StatusBarProps) {
  const { isMobile, open, openMobile, toggleSidebar } = useSidebar();

  return (
    <footer className="flex items-center justify-between gap-1 p-1 border-t">
      <Toggle
        size="sm"
        pressed={isMobile ? openMobile : open}
        onPressedChange={toggleSidebar}
        aria-label="Toggle Sidebar"
      >
        <PanelLeftIcon />
      </Toggle>

      <div className="flex items-center justify-end gap-1">
        <Toggle
          size="sm"
          pressed={activePanel === 'goals'}
          onPressedChange={onToggleGoals}
          aria-label="Goals panel"
        >
          <Target />
          {goalCount}
        </Toggle>

        <Toggle
          size="sm"
          pressed={activePanel === 'problems'}
          onPressedChange={onToggleProblems}
          aria-label="Problems panel"
        >
          <TriangleAlert />
          {problemCount}
        </Toggle>

        <Toggle
          size="sm"
          pressed={vim}
          onPressedChange={onVimChange}
          aria-label="Toggle Vim"
        >
          Vim
        </Toggle>

        <span className="flex font-mono italic font-bold items-center text-sm gap-1 select-none p-1">
          {problemCount === 0 && goalCount === 0 ? (
            <>
              QED
              <Check className="size-3.5" />
            </>
          ) : (
            <XIcon className="size-3.5" />
          )}
        </span>
      </div>
    </footer>
  );
}
