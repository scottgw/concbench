package ch.ethz.se.concbench.share;

import java.util.Vector;


public class Share {

	static class Worker implements Runnable {
		int [] shared;
		
		Worker (int[] s) {
			shared = s;
		}
		
		public void run() {
			for (int i = 0; i < arraySize; i++) {
				shared[i]++;
			}
		}
	}
	
	static final int arraySize = 5000;
	
	public static void main(String[] args) {
		int n = Integer.parseInt(args[0]);
		Vector<Thread> threads = new Vector<Thread>();
		int[] shared = new int[arraySize];

		for (int i = 0; i < n; i++) {
			Thread t = new Thread(new Worker(shared));
			threads.add(t);
			t.start();
		}

		for (Thread t : threads) {
			try {
				t.join();
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

}
