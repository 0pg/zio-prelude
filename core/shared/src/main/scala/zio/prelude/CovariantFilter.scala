package zio.prelude

import zio.Chunk
import zio.prelude.coherent.CovariantIdentityBoth

trait CovariantFilter[F[+_]] {
  def covariant: Covariant[F]
  def mapFilter[A, B](fa: F[A])(f: A => Option[B]): F[B]
  def collect[A, B](fa: F[A])(f: PartialFunction[A, B]): F[B] =
    mapFilter(fa)(f.lift)
}

object CovariantFilter {
  def apply[F[+_]](implicit covariantFilter: CovariantFilter[F]): CovariantFilter[F] =
    covariantFilter

  /**
   * The `ForEachFilter` (and thus `CovariantFilter`) for `Chunk`.
   */
  implicit val ChunkForEachFilter: ForEachFilter[Chunk] =
    new ForEachFilter[Chunk] {
      def forEachFilter[G[+_]: IdentityBoth: Covariant, A, B](fa: Chunk[A])(f: A => G[Option[B]]): G[Chunk[B]] =
        CovariantIdentityBoth[G].forEachFilter(fa)(f)

      def forEach: ForEach[Chunk] = Invariant.ChunkForEach
    }

  /**
   * The `ForEachFilter` instance for `Const`.
   */
  implicit def ConstForEachFilter[A]: ForEachFilter[({ type ConstA[+B] = Const[A, B] })#ConstA] =
    new ForEachFilter[({ type ConstA[+B] = Const[A, B] })#ConstA] {
      def forEachFilter[G[+_]: IdentityBoth: Covariant, B, C](
        fa: Const[A, B]
      )(f: B => G[Option[C]]): G[Const[A, C]] =
        Const.wrap(Const.unwrap(fa)).succeed

      def forEach: ForEach[Const[A, +*]] = Invariant.ConstForEach[A]
    }

  /**
   * The `ForEachFilter` (and thus `CovariantFilter`) instance for `List`.
   */
  implicit val ListForEachFilter: ForEachFilter[List] =
    new ForEachFilter[List] {
      def forEachFilter[G[+_]: IdentityBoth: Covariant, A, B](fa: List[A])(f: A => G[Option[B]]): G[List[B]] =
        CovariantIdentityBoth[G].forEachFilter(fa)(f)

      def forEach: ForEach[List] = Invariant.ListForEach
    }

  /**
   * The `ForEachFilter` (and thus `CovariantFilter`) instance for `Map`.
   */
  implicit def MapForEachFilter[K]: ForEachFilter[({ type lambda[+v] = Map[K, v] })#lambda] =
    new ForEachFilter[({ type lambda[+v] = Map[K, v] })#lambda] {
      def forEachFilter[G[+_]: IdentityBoth: Covariant, V, V2](map: Map[K, V])(f: V => G[Option[V2]]): G[Map[K, V2]] =
        CovariantIdentityBoth[G]
          .forEachFilter[(K, V), (K, V2), Iterable](map) { case (k, v) => f(v).map(_.map(k -> _)) }
          .map(_.toMap)

      def forEach: ForEach[Map[K, +*]] = Invariant.MapForEach[K]
    }

  /**
   * The `ForEachFilter` (and thus `CovariantFilter`) instance for `Option`.
   */
  implicit val OptionForEachFilter: ForEachFilter[Option] =
    new ForEachFilter[Option] {
      def forEachFilter[G[+_]: IdentityBoth: Covariant, A, B](option: Option[A])(f: A => G[Option[B]]): G[Option[B]] =
        option.fold[G[Option[B]]](Option.empty.succeed)(f)

      def forEach: ForEach[Option] = Invariant.OptionForEach
    }

  /**
   * The `ForEachFilter` (and thus `CovariantFilter`) instance for `Vector`.
   */
  implicit val VectorForEachFilter: ForEachFilter[Vector] =
    new ForEachFilter[Vector] {
      def forEachFilter[G[+_]: IdentityBoth: Covariant, A, B](
        as: Vector[A]
      )(f: A => G[Option[B]]): G[Vector[B]] =
        CovariantIdentityBoth[G].forEachFilter(as)(f)

      def forEach: ForEach[Vector] = Invariant.VectorForEach
    }

  /**
   * Derives a `ForEachFilter[F]` from an `Iterable[F]`.
   */
  implicit def IterableForEachFilter[F[+a] <: Iterable[a]](implicit
    derive: Invariant.DeriveBuildFrom[F]
  ): ForEachFilter[F] =
    new ForEachFilter[F] {
      def forEachFilter[G[+_]: IdentityBoth: Covariant, A, B](
        fa: F[A]
      )(f: A => G[Option[B]]): G[F[B]] =
        CovariantIdentityBoth[G].forEachFilter(fa)(f)(derive.derive)

      def forEach: ForEach[F] = Invariant.IterableForEach[F]
    }
}

trait CovariantFilterSyntax {
  implicit class CovariantFilterOps[F[+_], A](private val self: F[A]) {
    def mapFilter[B](f: A => Option[B])(implicit C: CovariantFilter[F]): F[B] =
      C.mapFilter(self)(f)

    def collect[B](f: PartialFunction[A, B])(implicit C: CovariantFilter[F]): F[B] =
      C.collect(self)(f)
  }
}
