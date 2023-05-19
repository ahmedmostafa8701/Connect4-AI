from board import Board
from pyswip import Prolog
import time
import random

# GAME LINK
# http://kevinshannon.com/connect4/


def main():
    board = Board()
    # Create a Prolog object
    prolog = Prolog()

    # Load the Prolog code file
    prolog.consult("alpha_beta.pl")
    time.sleep(4)
    game_end = False
    while not game_end:
        (game_board, game_end) = board.get_game_grid()
        # FOR DEBUG PURPOSES
        board.print_grid(game_board)
        transBoard = board._transpose_grid(game_board)
        progrid = board.boardToProlog(transBoard)
        colomn = list(prolog.query("alpha_beta(" + progrid + ", r, Column)"))[0]["Column"]
        # YOUR CODE GOES HERE

        # Insert here the action you want to perform based on the output of the algorithm
        # You can use the following function to select a column
        if colomn == 'null':
            game_end = True
            break
        board.select_column(colomn)

        time.sleep(2)


if __name__ == "__main__":
    main()