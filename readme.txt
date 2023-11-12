# Shoot 'em up

#### By: Frederique van Rijen (0695076) & Julian van Bijlert

## Controls

- Keys a,s,d,w can be used to go left, down, right and up
- key spacebar is used to Shoot
- Key esc can pause the game
- you can click on a running game resulting in the player moving towards that
- when in the menu you can click on where you wanna go
- you can save youre highscore by clicking save score after you have died

## Minimal requirements
  Player: - the data type is located at the model
          - the player moves and shoots with handleInput at the end of the controller file
  Enemies: - Their data types are located in model
           - Enemies can collided with the player and some can shoot at the player, making the player lose a heart
           - The enemies Jet and Mothership have the minimal intellegence to shoot at the player 
                with the MotherShip also moving towards the player
  Randomness: - the function makeRandomCoordinate uses a StdGen to make a random float
              - This float is used to randomly decide if an enemy will spawn (with the exeption if there are no enemies)
                 another random float is used to choose which enemy will appear and anther for where the enemy will appear
  Animation: - In view there is the showExplosion function that makes an explosion after an enemy has died
             - This explosion will last several frames where it keeps getting bigger
  Pause : the game can be paused using the key esc, which changes the state to paused
  Interaction with file system : - There is an interaction with the file system by saving the high score and showing the lists of highscores
                                 - The code for viewing the high scores are at the end of the view file
                                 - You can save the highscore after you die and you can see the high score by going to the menu

## Notes
- Holding down the keys a,s,d,w and spacebar does not mean the player keeps moving or keeps on shooting,
    the player then only moves/shoots one time
- In model there is a list of enemies commented, if you put that in start enemies, all the enemies appear at the beginning
  this may be needed to easily check out all the enemies since the tougher enemies appear later in the game
- You have three hearts and a shield, shown topleft of the screen
- If you touch the sides of the border or the screen its game over
