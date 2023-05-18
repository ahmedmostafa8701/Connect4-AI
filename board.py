from PIL import ImageGrab
import pyautogui

# YOU MAY NEED TO CHANGE THESE VALUES BASED ON YOUR SCREEN SIZE
LEFT = 551
TOP = 217
RIGHT = 1303
BOTTOM = 862

EMPTY = 0
RED = 1
BLUE = 2


class Board:
    def __init__(self) -> None:
        self.board = [[EMPTY for i in range(7)] for j in range(6)]

    def print_grid(self, grid):
        for i in range(0, len(grid)):
            for j in range(0, len(grid[i])):
                if grid[i][j] == EMPTY:
                    print("*", end=" \t")
                elif grid[i][j] == RED:
                    print("R", end=" \t")
                elif grid[i][j] == BLUE:
                    print("B", end=" \t")
            print("\n")
    def _convert_grid_to_color(self, grid):
        # Define threshold values for each color
        white_threshold = 200  # Adjust as needed
        blue_threshold = 100   # Adjust as needed
        red_threshold = 150    # Adjust as needed
        for i in range(0, len(grid)):
            for j in range(0, len(grid[i])):
                # Determine the color based on the RGB values and thresholds
                if grid[i][j][0] > red_threshold and grid[i][j][1] < blue_threshold and grid[i][j][2] < blue_threshold:
                    grid[i][j] = RED
                elif grid[i][j][0] < blue_threshold and grid[i][j][1] < blue_threshold and grid[i][j][2] > blue_threshold:
                    grid[i][j] = BLUE
                elif grid[i][j][0] > white_threshold and grid[i][j][1] > white_threshold and grid[i][j][2] > white_threshold:
                    grid[i][j] = EMPTY
                else:
                    grid[i][j] = EMPTY
        return grid

    def _get_grid_cordinates(self):
        startCord = (7, 5)
        cordArr = []
        for j in range(0, 6):
            for i in range(0, 7):
                #distance out side 8 top 9 right/ distance inside 18/ height 89
                x1 = startCord[0] + i * 107
                y1 = startCord[1] + j * 107
                x = x1 + 44.5
                y = y1 + 44.5
                cordArr.append((x, y))
        return cordArr

    def _capture_image(self):
        image = ImageGrab.grab()
        cropedImage = image.crop((LEFT, TOP, RIGHT, BOTTOM))
        return cropedImage

    def _convert_image_to_grid(self, image):
        pixels = [[] for i in range(6)]
        i = 0
        coordinates = self._get_grid_cordinates()
        for index, cord in enumerate(coordinates):
            pixel = image.getpixel(cord)
            if index % 7 == 0 and index != 0:
                i += 1
            pixels[i].append(pixel)
        return pixels

    def _get_grid(self):
        cropedImage = self._capture_image()
        #cropedImage.show()
        grid = self._convert_image_to_grid(cropedImage)
        return grid

    def _check_if_game_end(self, grid):
        for i in range(0, len(grid)):
            for j in range(0, len(grid[i])):
                if grid[i][j] == EMPTY and self.board[i][j] != EMPTY:
                    return True
        return False

    def get_game_grid(self):
        game_grid = self._get_grid()
        new_grid = self._convert_grid_to_color(game_grid)
        is_game_end = self._check_if_game_end(new_grid)
        self.board = new_grid
        return (self.board, is_game_end)

    def select_column(self, column):
        cell = 3 * 7 + column
        pyautogui.click(
            self._get_grid_cordinates()[cell][0] + LEFT,
            self._get_grid_cordinates()[cell][1] + TOP,
        )
    def boardToProlog(self, grid):
        progrid = "["
        for i in range(0, len(grid)):
            gridRow = "["
            for j in range(0, len(grid[i])):
                if grid[i][j] == EMPTY:
                    gridRow += "w"
                elif grid[i][j] == RED:
                    gridRow += "r"
                elif grid[i][j] == BLUE:
                    gridRow += "b"
                if j == len(grid[i]) - 1:
                    break
                gridRow += ", "
            gridRow += "]"
            progrid += gridRow
            if i == len(grid) - 1:
                break
            progrid += ", "
        progrid += "]"
        return progrid
    def _transpose_grid(self, grid):
        return [[grid[j][i] for j in range(len(grid))] for i in range(len(grid[0]))]

