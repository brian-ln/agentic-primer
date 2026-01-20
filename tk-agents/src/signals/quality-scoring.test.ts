
import { expect, test, describe } from "bun:test";
import { QualityScoring } from "./quality-scoring.ts";

describe("QualityScoring", () => {
  const scoring = new QualityScoring();

  test("should give high score to long, technical AI videos", () => {
    const metadata = {
      title: "Building Autonomous AI Agents with Transformer Architecture",
      description: "A deep dive into transformer models, fine-tuning, and RLHF for agentic reasoning.",
      duration: 3600, // 60 mins
      publishedAt: new Date().toISOString(),
      url: "https://youtube.com/watch?v=mock",
      videoId: "mock",
      views: 10000,
      channel: "AI Research",
      thumbnail: "https://mock.com/thumb.jpg",
      hasTranscript: true,
    };

    const result = scoring.calculateScore(metadata);
    expect(result.total).toBeGreaterThan(0.7);
    expect(result.breakdown.topic).toBe("ai-agents");
  });

  test("should give low score to short, non-technical videos", () => {
    const metadata = {
      title: "Funny Cat Video",
      description: "Just a cat doing things.",
      duration: 60, // 1 min
      publishedAt: new Date().toISOString(),
      url: "https://youtube.com/watch?v=mock2",
      videoId: "mock2",
      views: 1000000,
      channel: "Cats",
      thumbnail: "https://mock.com/thumb2.jpg",
      hasTranscript: false,
    };

    const result = scoring.calculateScore(metadata);
    expect(result.total).toBeLessThan(0.4);
  });

  test("should penalize old videos on fast-moving topics", () => {
    const publishedAt = new Date();
    publishedAt.setFullYear(publishedAt.getFullYear() - 2);

    const metadata = {
      title: "AI Agents Update",
      description: "News from 2 years ago.",
      duration: 1200,
      publishedAt: publishedAt.toISOString(),
      url: "https://youtube.com/watch?v=mock3",
      videoId: "mock3",
      views: 5000,
      channel: "AI News",
      thumbnail: "https://mock.com/thumb3.jpg",
      hasTranscript: true,
    };

    const result = scoring.calculateScore(metadata);
    expect(result.breakdown.recency).toBeLessThan(0);
  });
});
