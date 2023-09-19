import random
from itertools import combinations
import time
import sys
from backgorund_board import QueenBoard



class NQueens:
    exec_solutions = 0
    max_iterations = 0
    queens_quantity = 0
    movement_time = 0

    def __init__(self, max_iterations, queens_quantity, movement_time):
        self.max_iterations = max_iterations
        self.queens_quantity = queens_quantity
        self.movement_time = movement_time

    def objective_func(self, candidate_solution, size_solution):
        positive_diagonal = [0 for _ in range(size_solution)]
        negative_diagonal = [0 for _ in range(size_solution)]

        for index in range(size_solution):
            k_positive = index - candidate_solution[index]
            k_negative = index + candidate_solution[index]

            positive_diagonal[index] = k_positive
            negative_diagonal[index] = k_negative

        fit_solution = 0
        fit_solution += (len(positive_diagonal) - len(set(positive_diagonal)))
        fit_solution += (len(negative_diagonal) - len(set(negative_diagonal)))

        return fit_solution

    def generate_tabu_matrix(self, size_combinations):
        tabu_matrix = [[0 for _ in range(size_combinations)], [0 for _ in range(size_combinations)]]
        return tabu_matrix

    def generate_next_move(self, current_solution, current_fit, possible_moves, size_moves, tabu_matrix, n_iteration):
        candidate_solution = current_solution.copy()
        neighbor_solutions = []
        # Retorna os indices de movimentos possíveis de acordo com MOVEMENT_TIME
        index_moves = [index for index in range(size_moves) if tabu_matrix[0][index] < n_iteration]

        for move in index_moves:
            candidate_solution[possible_moves[move][0]] = current_solution[possible_moves[move][1]]
            candidate_solution[possible_moves[move][1]] = current_solution[possible_moves[move][0]]

            candidate_fit = self.objective_func(candidate_solution, self.queens_quantity)

            if candidate_fit < current_fit:
                # tabu matrix[0] armazena tempo de congelamento; tabu matrix[1] quantidade de vezes do movimento
                tabu_matrix[0][move] += n_iteration + self.movement_time
                tabu_matrix[1][move] += 1

                return candidate_solution, candidate_fit, tabu_matrix
            else:
                if candidate_fit == current_fit:
                    neighbor_solutions.append([candidate_solution, candidate_fit, move])

                candidate_solution = current_solution.copy()

        if len(neighbor_solutions):
            neighbor_candidate_solution = random.sample(neighbor_solutions, k=1)[0]

            move = neighbor_candidate_solution[2]
            tabu_matrix[0][move] += n_iteration + self.movement_time
            tabu_matrix[1][move] += 1

            return neighbor_candidate_solution[0], neighbor_candidate_solution[1], tabu_matrix
        else:
            min_move = min(tabu_matrix[1])
            min_move_index = tabu_matrix[1].index(min_move)
            tabu_matrix[1][min_move_index] += 1

            candidate_solution[possible_moves[min_move_index][0]] = current_solution[possible_moves[min_move_index][1]]
            candidate_solution[possible_moves[min_move_index][1]] = current_solution[possible_moves[min_move_index][0]]

            candidate_fit = self.objective_func(candidate_solution, self.queens_quantity)

            return candidate_solution, candidate_fit, tabu_matrix

    def tabu_queens(self):

        possible_moves = list(combinations(list(range(self.queens_quantity)), r=2))
        size_moves = len(possible_moves)

        initial_seconds = time.time()

        best_solution = random.sample(range(0, self.queens_quantity), self.queens_quantity)
        best_fit = self.objective_func(best_solution, self.queens_quantity)

        tabu_matrix = self.generate_tabu_matrix(size_moves)

        n_iteration = 0
        while n_iteration < self.max_iterations and best_fit != 0:
            n_iteration += 1

            best_solution, best_fit, tabu_matrix = self.generate_next_move(
                best_solution, best_fit, possible_moves, size_moves, tabu_matrix, n_iteration)

        final_seconds = time.time() - initial_seconds

        print('-------------------------------')
        print('segundos', final_seconds)
        print('iterações', n_iteration)
        print('fit final:', best_fit)
        print('solução final', best_solution)
        print('-------------------------------')
        return best_solution


if __name__ == '__main__':


    if sys.argv[1] and sys.argv[2] and sys.argv[3]:
        MAX_ITERATIONS =  int(sys.argv[1])
        QUEENS_QUANTITY = int(sys.argv[2])
        MOVEMENT_TIME = int(sys.argv[3])

        # -- Tabuleiro -- #
        SIZE = 480
        BLUE = (75, 68, 200)
        WHITE = (255, 255, 255)

        n_queens = NQueens(max_iterations=MAX_ITERATIONS, queens_quantity=QUEENS_QUANTITY,
                        movement_time=MOVEMENT_TIME)
        best_sol = n_queens.tabu_queens()

        board = QueenBoard((SIZE, SIZE), SIZE / QUEENS_QUANTITY, 'imagens/queen.png', BLUE, WHITE)
        board.loop(QUEENS_QUANTITY, best_sol)

    else:
        print('Informe o numero máximo de iterações, quantidade de rainhas e tempo tabu (python main.py iterações qtd_rainhas tempo_tabu')