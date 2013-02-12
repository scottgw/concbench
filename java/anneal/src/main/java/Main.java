package ch.ethz.se.concbench.anneal;

import java.util.Vector;
import java.util.concurrent.CyclicBarrier;

public class Main {
	static final int size = 1000;
	static final double maxDist = 200.0;
	static final int testRuns = 10;
	
	public static void main (String[] args) {
		int n = Integer.parseInt(args[0]);
		
		Map map = new Map(size, maxDist);
		System.out.println("Map done");
		
		long t1 = System.currentTimeMillis();
		for (int i = 0; i < testRuns; i++) {
			test (n, map);
		}
		long t2 = System.currentTimeMillis();
		
		System.out.println(((double)(t2-t1)/testRuns/1000.0) + " s");
	}
	

	public static void test (int n, Map map) {
		Route route = new Route (size, map);
//		System.out.println("Distance " + route.distance());
		
		CyclicBarrier barrier = new CyclicBarrier(n);
		Vector<Worker> workers = new Vector<Worker>();
		Vector<Thread> threads = new Vector<Thread>();
		for (int i = 0; i < n; i++){
			Worker w = new Worker(i, map, route, barrier);
			Thread t = new Thread (w);
			workers.add(w);
			threads.add(t);
			t.start();
		}
		
		for (Thread t : threads) {
			try {
				t.join();
			} catch (InterruptedException e) {
				System.out.println("Exception while waiting on join.");
				e.printStackTrace();
			}
		}
//		System.out.println("Distance " + route.distance() + 
//				" and isValid: " + route.isValid());
	}
}
