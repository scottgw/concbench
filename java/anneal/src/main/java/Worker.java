package ch.ethz.se.concbench.anneal;

import static java.lang.Math.*;
import java.util.Random;
import java.util.concurrent.CyclicBarrier;

public class Worker implements Runnable {
	enum Decision {Good, Bad, Deny};
	
	Worker (int seed,
			Map map,
			Route route,
			CyclicBarrier barrier) {
		this.seed = seed;
		this.map = map;
		this.route = route;
		this.barrier = barrier;
		
		rand = new Random (seed);
		outerIters = 64;
		innerIters = 1000;
	}
	
	
	int outerIters;
	int innerIters;
	int seed;
	Map map;
	Route route;
	CyclicBarrier barrier;
	Random rand;

	@Override
	public void run () {
		double deltaDist;
		double temper;
		City a, b;
		int good, bad;		
		good = 0;
		bad = -1;
		
		temper = 200;
		
		a = randomCity ();
		
		for (int i = 0; keepGoing (i, good, bad, temper); i++) {
			for (int j = 0; j < innerIters; j++) {
				b = randomCity ();
				
				deltaDist = calculateDeltaDist (a, b);
				Decision decision = decide (deltaDist, temper);
				
				if (decision == Decision.Good) {
					good++;
					route.swap (a, b);
				} else if (decision == Decision.Bad) {
					bad++;
					route.swap(a, b);
				}
				
				a = b;
			}
			
			temper = cool (temper);
		}
	}
	
	double singleDist (City a) {
		double dist = 0.0;
		
		if (a.leftId != -1) {
			dist += map.map [a.leftId][a.id];
		}

		if (a.rightId != -1) {
			dist += map.map [a.id][a.rightId];
		}
		
		
		return dist;
	}
	
	double calculateDeltaDist (City a, City b) {
		City newA = new City (a.id, b.indexInRoute, b.leftId, b.rightId);
		City newB = new City (b.id, a.indexInRoute, a.leftId, a.rightId);
		
		double dist = singleDist (newA) + singleDist (newB);
		dist -= singleDist (a) + singleDist (b);
		
		return dist;
	}
	
	City randomCity () {
		int i = rand.nextInt(route.size());
		return route.getCity(i);
	}
	
	boolean keepGoing (int i, int good, int bad, double temper) {
		return i < outerIters;
	}
	
	Decision decide (double dist, double temper) {
		double rnd = rand.nextDouble();
		
		if (dist < 0) {
			return Decision.Good;
		} else if (exp (-dist/temper) > rnd) {
			return Decision.Bad;
		} else {
			return Decision.Deny;
		}
	}
	
	double cool (double t) {
		return t / 1.5;
	}
}
