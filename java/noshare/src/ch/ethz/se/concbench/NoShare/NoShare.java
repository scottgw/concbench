package ch.ethz.se.concbench.NoShare;

import java.util.Vector;

public class NoShare {

	static class Worker implements Runnable {
		public void run () {
			fib (40);
		}
		
		int fib (int i) {
			if (i <= 1)
				return i;
			else
				return fib (i-1) + fib (i-2);
		}
	}

	public static void main (String args[]) {
		int numEach = Integer.parseInt(args[0]);
		Vector <Thread> threads = new Vector<Thread> ();
		
		for (int i = 0; i < numEach; i++) {
			Thread t = new Thread (new Worker());
			t.start();
			threads.add(t);
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
