# ludolib
A library of game utilities. I'm mostly going to develop them with 2D games in mind.

[![License AGPL][badge-license]][license]
[badge-license]: https://img.shields.io/badge/license-AGPL-green.svg
[license]: https://github.com/Lokathor/ludolib/blob/master/AGPL.txt

## Current and Planned Features

- [x] A [Permuted Linear Congruential Generator](http://www.pcg-random.org/) implementation, with a [RandomGen](https://hackage.haskell.org/package/random-1.1/docs/System-Random.html#t:RandomGen) instance, for all your PRNG needs.
- [x] Functions based on the [MonadRandom](https://hackage.haskell.org/package/MonadRandom-0.4.2.2/docs/Control-Monad-Random-Class.html#t:MonadRandom) typeclas for rolling all sorts of dice rolls.
- [x] [Precise Permissive Field of View](http://www.roguebasin.com/index.php?title=Precise_Permissive_Field_of_View) calculation.
- [ ] Dungeon Generation: Cellular Automata, Herringbone Wang Tiles, and others.
- [ ] Pathfinding: Both [A*](https://en.wikipedia.org/wiki/A*_search_algorithm) ("A star") pathfinding, as well as [Dijkstra Maps](http://www.roguebasin.com/index.php?title=The_Incredible_Power_of_Dijkstra_Maps).
