# c3110-final
Repo for the 3110 final project

Group: Sunny Chavan, Corban Chiu, Connor McCarthy

## Monopoly Proposal
### Alpha (board but not really a game)
**S:** Board definition (factored out into JSON)
**G:** GUI that displays the above
**E:** Dice rolls, money, movement

### Beta (game with "playability")
**S:** Buying properties (auctions), collecting rent, mortgaging, jail
**G:** Community Chest and Chance (factored into JSON), build houses and hotels,
 free parking
**E:** Multiplayer playable

### Release (real game)
**S:** Multi-person playable, trading properties
**G:** Game menu, win condition
**E:** Bot with different difficulty levels

## Internal Use of Code
- Upon running `make test`, a file `/_coverage/index.html` is created. Opening
this file will show the disect code coverage of the test file.

## Installation Instructions
Mac OS: sudo port install pkgconfig

Windows: apt install pkg-config

Graphics Package: opam install graphics
