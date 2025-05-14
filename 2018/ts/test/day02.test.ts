import { describe, expect, test } from '@jest/globals';
import { partA } from "../src/day02";

describe("2018", () => {
  describe("Day02", () => {
    test("partA sample", async () => {
      const boxes = [
        "abcdef",
        "bababc",
        "abbcde",
        "abcccd",
        "aabcdd",
        "abcdee",
        "ababab"
      ];
      const result = partA(boxes);
      expect(result).toBe(12);
    });
  });
});

