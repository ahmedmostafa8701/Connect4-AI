from board import Board
from pyswip import Prolog
import time
import random
import tkinter as tk

# GAME LINK
# http://kevinshannon.com/connect4/

root = tk.Tk()
root.title("Color and Difficulty Choices")
root.geometry("400x400")

# Color Selection
color_frame = tk.LabelFrame(root, text="Color")
color_frame.pack(padx=10, pady=10)

color_var = tk.StringVar()
color_var.set("red")  # Set default value

color_radio_red = tk.Radiobutton(color_frame, text="Red", variable=color_var, value="red")
color_radio_red.pack(anchor="w")

color_radio_blue = tk.Radiobutton(color_frame, text="Blue", variable=color_var, value="blue")
color_radio_blue.pack(anchor="w")

# Difficulty Selection
difficulty_frame = tk.LabelFrame(root, text="Difficulty")
difficulty_frame.pack(padx=10, pady=10)

difficulty_var = tk.StringVar()
difficulty_var.set("easy")  # Set default value

difficulty_radio_easy = tk.Radiobutton(difficulty_frame, text="Easy", variable=difficulty_var, value="easy")
difficulty_radio_easy.pack(anchor="w")

difficulty_radio_medium = tk.Radiobutton(difficulty_frame, text="Medium", variable=difficulty_var, value="medium")
difficulty_radio_medium.pack(anchor="w")

difficulty_radio_hard = tk.Radiobutton(difficulty_frame, text="Hard", variable=difficulty_var, value="hard")
difficulty_radio_hard.pack(anchor="w")
def main():
    board = Board()
    # Create a Prolog object
    prolog = Prolog()
    color_choice , difficulty_choice = ui()
    # Load the Prolog code file
    prolog.consult("alpha_beta.pl")
    time.sleep(2)
    game_end = False
    while not game_end:
        color = ''
        diff = 2
        if color_choice == 'red' :
            color = 'r'
        else :
            color = 'b'
        (game_board, game_end) = board.get_game_grid()
        # FOR DEBUG PURPOSES
        board.print_grid(game_board)
        transBoard = board._transpose_grid(game_board)
        progrid = board.boardToProlog(transBoard)
        # YOUR CODE GOES HERE
        if difficulty_choice == 'easy':
            colomn = list(prolog.query("minMax(" + progrid + ", " + color + ", 3, Column)"))[0]["Column"]
        elif difficulty_choice == 'medium' :
            colomn = list(prolog.query("minMax(" + progrid + ", " + color + ", 5, Column)"))[0]["Column"]
        else :
            colomn = list(prolog.query("alpha_beta(" + progrid + ", " + color + ", Column)"))[0]["Column"]
        # Insert here the action you want to perform based on the output of the algorithm
        # You can use the following function to select a column
        if colomn == 'null':
            game_end = True
            break
        board.select_column(colomn)
        time.sleep(10)
def get_choices():
    global color_choice, difficulty_choice
    color_choice = color_var.get()
    difficulty_choice = difficulty_var.get()
    root.destroy()
def ui():
    global color_choice, difficulty_choice
    submit_button = tk.Button(root, text="Submit", command=get_choices)
    submit_button.pack(pady=10)
    root.mainloop()
    return color_choice, difficulty_choice
    

if __name__ == "__main__":
    main()