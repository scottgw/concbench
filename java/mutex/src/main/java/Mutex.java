package ch.ethz.se.concbench.mutex;

import java.util.Vector;

public class Mutex {
	static class Shared {
		int x;

		Shared() {
			x = 0;
		}
		
		synchronized void update () {
			x++;
		}
	}

	static class Worker implements Runnable {
		Shared shared;
		static final int maxIters = 20000;

		public Worker(Shared s) {
			shared = s;
		}

		public void run() {
			for (int i = 0; i < maxIters; i++) {
				shared.update();
			}
		}
	}

	static Shared shared;

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		int n = Integer.parseInt(args[0]);
		shared = new Shared();
		Vector<Thread> workers = new Vector<Thread>();

		for (int i = 0; i < n; i++) {
			Thread worker = new Thread(new Worker(shared));
			workers.add(worker);
			worker.start();
		}

		try {
			for (Thread t : workers) {
				t.join();
			}
		} catch (Exception e) {
			e.printStackTrace();
		}

	}

}
