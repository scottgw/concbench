%include polycode.fmt
%format :+: = "\mathbf{\plus}"
%format :*: = "\times"
%format >>> = "\ggg"
%format *** = "\sss"
%format &&& = "\aaa"
%if False
\begin{code}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
module CustomBench where

import Control.Category

import Flow
\end{code}
%endif

The foundations given in the previous section
allow the definition of benchmarks which
extend the simple foundations.
As an example,
one may wish to have a matrix multiplcation
microbenchmark.
This can be done by defining a new
interface |MatrixArrow|,
\begin{code}
class BenchArrow arrow => MatrixArrow arrow 
 where
\end{code}
which internally can be parameterized by
the type of the |Matrix|.
In the end, this means that different
implementations can represent this type differently:
whether by a proper two dimensional array,
or a one dimensional array.
\begin{code}
  type Matrix arrow :: *
\end{code}
The interface defines a way to generate 
a new matrix
\begin{code}
  genMatrix :: arrow a (Matrix arrow)
\end{code}
and a matrix multiplication operation
that will be benchmarked.
\begin{code}
  matMul :: 
    arrow (Matrix arrow :*: Matrix arrow) (Matrix arrow)
\end{code}
The above interface represents a useful pattern:
the type of data the benchmark operates on,
how it is generated,
and how it is used are all
included as abstract notions in the interface,
and open to be defined to particular instances
by the implementer.


Specific benchmarks can then be composed from these pieces:
below we see an example of generating and multiplying
several matrices in parallel.
First, two matrices are generated in parallel.
This pair of matrices are then given to
two parallel multiplcation operations
the result of which is then submitted
again to a matrix multiplication operation.
This example makes it clear that these
are not data-flow arrows, but type-flow arrows,
as there is no distinction between the same
data and the same \emph{type} of data,
although sometimes they can coincide.
\begin{code}
doubleMul :: MatrixArrow arrow => 
             arrow a (Matrix arrow)
doubleMul =
  (genMatrix &&& genMatrix) >>> 
  (matMul &&& matMul) >>> 
  matMul
\end{code}

It is also possible to define operations which
are not computationally intensive,
yet are useful to define other benchmarks.
For instance,
one can define the lock type,
generator,
and operations in a similar way.
\begin{code}
class LockArrow arrow where
  type Lock arrow :: *
  genLock :: arrow a (Lock arrow)
  lock :: arrow (Lock arrow) (Lock arrow)
  unlock :: arrow (Lock arrow) (Lock arrow)
\end{code}
The |lock| and |unlock| operations perform the
expected tasks, and should be used to guard computations.,
Usage of these arrows can be seen below where
|protect| constructs an arrow that 
requires, in addition to the normal argument, a |Lock|.
\begin{code}
protect :: (Arrow arrow, LockArrow arrow) =>
           arrow a b ->
           arrow (Lock arrow :*: a) (Lock arrow :*: b)
protect f = first lock >>> second f >>> first unlock
\end{code}
|protect| can then be used to construct
a benchmark that,
given two benchmarks requiring the same type of input,
will safely execute both in parallel.
Notice the |Lock| type does not appear here
as it was generated and forgotten within the function.
\begin{code}
safeShare :: (BenchArrow arrow, LockArrow arrow) =>
              arrow a b -> arrow a c -> arrow a (b :*: c)
safeShare f g = 
  share >>> first genLock >>> (protect' f &&& protect' g)
 where
   protect' h = protect h >>> first sink >>> forgetEnd
\end{code}

% Since the embedding is well-typed, we need a way
% to transmit arbitrary types from the target language
% to our embedding.
% This is done by enumerating the types with
% natural numbers, as below.
% There are also constructors for 
% product and sum types,
% as well as ``special'' types that signify the end
% of a benchmark and the time reporting as a
% double precision floating point number.

