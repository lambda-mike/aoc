def partA(boxes):
    twoCount = 0
    threeCount = 0
    for box in boxes:
        boxHasTwoLetters = False
        boxHasThreeLetters = False
        letters = {}
        for char in box:
            if char in letters:
                letters[char] += 1
            else:
                letters[char] = 1
        for char, num in letters.items():
            if num == 2 and not boxHasTwoLetters:
                boxHasTwoLetters = True
                twoCount += 1
            if num == 3 and not boxHasThreeLetters:
                boxHasThreeLetters = True
                threeCount += 1
            if boxHasThreeLetters and boxHasTwoLetters:
                break
    return twoCount * threeCount

def partB(boxes):
    allBoxSubsets = set()
    for box in boxes:
        boxSubsets = set()
        for i, _ in enumerate(box):
            boxSubsets.add(box[:i] + box[i+1:])
        answerSet = allBoxSubsets & boxSubsets
        if len(answerSet) == 1:
            return list(answerSet)[0]
        allBoxSubsets.update(boxSubsets)

def main():
    file = "input.txt"
    boxes = []
    with open(file) as f:
        for line in f:
            boxes.append(line.strip())
    print("Solving Day02A...")
    a = partA(boxes)
    print(a)
    print("Solving Day02B...")
    b = partB(boxes)
    print(b)

main()
