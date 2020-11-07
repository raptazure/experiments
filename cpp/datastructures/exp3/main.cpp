#include <stdio.h>
bool relations[1000][1000] = {};

// Dijkstra's algorithm relies on the property that the shortest path from 𝑠 to 𝑡 is also the shortest path to any of the vertices along the path.
// If all the weights were 1, this is exactly what BFS does.

// implementing Dijkstra's algorithm as BFS with a queue(?:)

double bfs(int i, int N)
{
  int queue[1000] = {};
  int visited[1000] = {};
  int front = -1, rear = -1;
  queue[++rear] = i;
  // original ones enter the queue
  visited[i] = true;
  // leavePoint - of the queue
  int leavePoint, lastOfLayer = rear, layer = 0, num = 1;
  while (front != rear && layer < 6)
  {
    leavePoint = queue[++front];
    for (int j = 0; j < N; j++)
      if (!visited[j] && relations[leavePoint][j])
      {
        queue[++rear] = j;
        visited[j] = true;
        num++;
      }
    // this layer is visited, jump to the next one
    if (front == lastOfLayer)
    {
      layer++;
      lastOfLayer = rear;
    }
  }
  return (double)num * 100 / N;
}

int main()
{
  int N, M;
  scanf("%d%d", &N, &M);
  int v, e;
  for (int i = 0; i < M; i++)
  {
    scanf("%d%d", &v, &e);
    relations[v - 1][e - 1] = relations[e - 1][v - 1] = 1;
  }
  double res;
  for (int i = 0; i < N; i++)
  {
    res = bfs(i, N);
    printf("%d: %.2f\n", i + 1, res);
  }
  return 0;
}