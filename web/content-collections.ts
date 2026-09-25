import { defineCollection, defineConfig } from '@content-collections/core'
import { z } from 'zod'

const puzzles = defineCollection({
  name: 'puzzles',
  directory: '../docs/puzzles',
  include: '**/*.md',
  schema: z.object({
    title: z.string(),
    topic: z.string(),
    order: z.number().int(),
  }),
})

export default defineConfig({
  content: [puzzles],
})
