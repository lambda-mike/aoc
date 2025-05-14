import { describe, expect, it } from '@jest/globals';
import { introMsg } from "../src/intro";

describe("Test Suite", () => {
  describe("Intro", () => {
    it("should work", async () => {
      const result = introMsg();
      expect(result).toBe("ok");
    });
  });
});
