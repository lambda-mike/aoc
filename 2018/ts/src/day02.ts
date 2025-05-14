import { readFileSync } from "node:fs";

type Boxes = Array<string>;
type Checksum = number;

export const readInput = (file: string = "./src/input.txt"): Boxes => {
  const data = readFileSync(file);
  const str = data.toString();
  return str.split("\n");
}

export const partA = (boxes: Boxes): Checksum => {
  // keep number of 2 and 3 times occurances for all boxes
  const occurances = { 2: 0, 3: 0 };
  for (const box of boxes) {
    // count letter frequency for each box
    const lettersCount: Record<string, number> = {};
    for (const c of box) {
      if (c in lettersCount) {
        lettersCount[c] += 1;
      } else {
        lettersCount[c] = 1;
      }
    }
    let countTwo = false;
    let countThree = false;
    // increase 2 and 3 times occurances counter as necessary
    for (const [k, v] of Object.entries(lettersCount)) {
      if (v === 3 && !countThree) {
        countThree = true;
        occurances[3] += 1;
      }
      else if (v === 2 && !countTwo) {
        countTwo = true;
        occurances[2] += 1;
      }
      if (countTwo && countThree) {
        // we do not care about repetitions of two and three times occurances per single box
        break;
      }
    }
  }
  return occurances[2] * occurances[3];
}

export const partB = (boxes: Boxes): string | null => {
  const oneLetterLessStrings: Set<string> = new Set();
  for (const box of boxes) {
    const letters = box.split("");
    // we do not want duplicate sequences from the same box
    const boxOneLessLetterSet = new Set<string>();
    for (let i = 0; i < letters.length; i++) {
      let lettersCopy = [...letters];
      lettersCopy.splice(i, 1);
      const oneLessLetters = lettersCopy.join("");
      boxOneLessLetterSet.add(oneLessLetters);
    }
    for (const str of boxOneLessLetterSet) {
      // as soon as we encounter the same key we are done
      if (oneLetterLessStrings.has(str)) {
        return str;
      }
      oneLetterLessStrings.add(str);
    }
  }
  return null;
}

export const main = () => {
  const boxes = readInput();
  console.log("Solving part A...");
  const a = partA(boxes);
  console.log(a);
  console.log("Solving part B...");
  const b = partB(boxes);
  console.log(b);
};
