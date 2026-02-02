# Playground: Minimal Prolog Text Adventure

This is a proof-of-concept text adventure game written in Prolog for testing the plunit_assert library.

## Game Description
- There are two rooms: **A** and **B**.
- Only room **B** contains the treasure.
- You start in a random room.
- You can enter commands:
  - `move a` — move to room A
  - `move b` — move to room B
  - `pick treasure` — try to pick up the treasure
  - `look` — look around to see if there is treasure in the room
  - `where` — find out which room you are currently in
  - `quit` — exit the game
- If you are in room B and pick the treasure, you win!
- If you are not in the treasure room, you get another chance.

## How to Play

1. Make sure you have SWI-Prolog installed.
2. Load the game in SWI-Prolog:

   ```prolog
   ?- [playground/playground].
   ?- play.
   ```

3. Follow the prompts and enter your commands as strings (e.g., `move a`).

## Example Session

```
Welcome to Playground!
You are in room A. There is nothing here.
What do you want to do? (move a | move b | pick treasure | look | where | quit)
look
You look around. There is nothing of interest.
where
You are currently in room a.
move b
You move to room B.
You are in room B. There is a treasure here!
What do you want to do? (move a | move b | pick treasure | look | where | quit)
look
You look around and see a treasure here!
pick treasure
Congratulations! You found the treasure and win!
What do you want to do? (move a | move b | pick treasure | look | where | quit)
quit
Thanks for playing! Goodbye.
```

## Purpose

This game is intended as a simple, real Prolog codebase for testing and demonstrating the plunit_assert library.
