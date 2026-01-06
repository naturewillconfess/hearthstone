hearthstone
===========

A Python library for computing Nash equilibria in Hearthstone tournament formats.

This package calculates optimal mixed strategies for deck selection in competitive
Hearthstone tournaments, supporting both **Conquest** and **Last Hero Standing (LHS)**
formats, with optional ban phases.

Installation
-------------

To install the latest development version from GitHub:

.. code-block:: bash

   git clone https://github.com/savakian/hearthstone.git
   cd hearthstone
   pip install .

Quick Example
-------------

.. code-block:: python

   import numpy as np
   from hearthstone import conquest_nash

   # Winrate matrix: W[i,j] = P(your deck i beats opponent's deck j)
   W = np.array([
       [0.55, 0.45, 0.60],
       [0.50, 0.50, 0.50],
       [0.40, 0.55, 0.45],
   ])

   result = conquest_nash(W)
   initial = result[-1]

   print(f"Match win probability: {initial['winrate'][0]:.2%}")
   print(f"Optimal deck selection: {initial['nash'][0]}")

Contents
--------

.. toctree::
   :maxdepth: 2
   quickstart

.. toctree::
   :maxdepth: 2

   theory
   formats/index

.. toctree::
   :maxdepth: 2

   examples

.. toctree::
   :maxdepth: 2

   reference