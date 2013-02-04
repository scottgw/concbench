package ch.ethz.se.concbench.anneal;

import java.util.Random;

public class Map {
	double[][] map;
	
	Map (int size, double maxDist) {
		map = new double[size][size];
		Random rand = new Random();
		
		for (int i = 0; i < size; i++) {
			for (int j = 0; j < size; j++) {
				map[i][j] = rand.nextDouble() * maxDist;
			}
		}
	}
	
	double dist (City a, City b) {
		return map[a.id][b.id];
	}
}
