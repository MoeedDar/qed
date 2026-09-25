import { allPuzzles } from 'content-collections';

export type Puzzle = (typeof allPuzzles)[number];

const QED = /```qed\n([\s\S]*?)```/;

export function code(puzzle: Puzzle): string {
  const match = puzzle.content.match(QED);
  if (!match) {
    throw new Error(`puzzle ${slug(puzzle)} has no qed block`);
  }
  return `${match[1].trimEnd()}\n`;
}

export function slug(puzzle: Puzzle): string {
  return puzzle._meta.path;
}

export function ordered(): Puzzle[] {
  return [...allPuzzles].sort((a, b) => a.order - b.order);
}

export function bySlug(id: string): Puzzle | undefined {
  return allPuzzles.find((puzzle) => slug(puzzle) === id);
}

export function byTopic(): { topic: string; puzzles: Puzzle[] }[] {
  const topics = new Map<string, Puzzle[]>();
  for (const puzzle of ordered()) {
    const group = topics.get(puzzle.topic);
    if (group) {
      group.push(puzzle);
    } else {
      topics.set(puzzle.topic, [puzzle]);
    }
  }
  return [...topics].map(([topic, puzzles]) => ({ topic, puzzles }));
}
