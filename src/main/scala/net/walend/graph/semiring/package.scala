package net.walend.graph

/**
 * Semirings and semiring-based graph minimizing algorithms.
 *
 * SemiringSupport is the primary trait. Algorithms in this package -- Floyd-Warshall, Dijkstra's, and Brandes'
 * algorithms -- use your choice of SemiringSupport to determine just what they are minimizing. The package also
 * includes some common SemiringSupport implementations.
 *
 * @author dwalend
 * @since v0.1.0
 */
package object semiring {

}
//todo pathcount as a decorator semiring
//todo core-and-many-decorator semirings
//todo flag in SemiringSupport for where Dijkstra's algorithm is OK