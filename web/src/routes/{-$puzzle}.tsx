import { createFileRoute, notFound } from '@tanstack/react-router';

import { CodeEditor } from '@/components/code-editor';
import { Goals } from '@/components/goals';
import { Layout } from '@/components/layout';
import { Problems } from '@/components/problems';
import { AppSidebar } from '@/components/sidebar';
import { StatusBar } from '@/components/status-bar';
import { useEditorState } from '@/hooks/use-editor-state';
import { bySlug, code, slug } from '@/lib/puzzles';

export const Route = createFileRoute('/{-$puzzle}')({
  component: RouteComponent,
  loader: ({ params }) => {
    const id = params.puzzle;

    if (!id) {
      return {};
    }

    const puzzle = bySlug(id);

    if (!puzzle) {
      throw notFound();
    }

    return { puzzle };
  },
  ssr: false,
});

function RouteComponent() {
  const { puzzle } = Route.useLoaderData();

  const state = useEditorState(puzzle ? code(puzzle) : '');

  return (
    <Layout
      editor={
        <CodeEditor
          key={puzzle ? slug(puzzle) : 'playground'}
          code={state.code}
          onCodeChange={state.setCode}
          extensions={state.extensions}
          theme={state.theme}
          onCreateEditor={state.onCreateEditor}
          onUpdate={state.onUpdate}
        />
      }
      panel={
        state.activePanel === 'goals' ? (
          <Goals
            goals={state.goals}
            current={state.current}
            onSelect={(goal) => state.jumpToPosition(goal.start)}
          />
        ) : (
          <Problems problems={state.problems} onJump={state.jumpTo} />
        )
      }
      showPanel={state.showPanel}
      sidebar={<AppSidebar />}
      statusBar={
        <StatusBar
          vim={state.vim}
          onVimChange={state.setVim}
          activePanel={state.activePanel}
          onToggleGoals={state.toggleGoals}
          onToggleProblems={state.toggleProblems}
          goalCount={state.goals.length}
          problemCount={state.problems.length}
        />
      }
    />
  );
}
