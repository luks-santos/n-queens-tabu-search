import pygame


class BoardBackground:
    color_1 = None
    color_2 = None
    image = None
    tile_size = 0
    screen = None

    def __init__(self, image, color_1, color_2, tile_size, ):
        self.color_1 = color_1
        self.color_2 = color_2
        self.image = pygame.image.load(image)
        self.tile_size = tile_size
        self.image = pygame.transform.scale(self.image, (self.tile_size, self.tile_size))
        self.screen = pygame.display.get_surface()

        background = pygame.Surface(self.screen.get_size()).convert()
        background.fill(color_1)

    def draw(self, queens_quantity, solution):
        for row in range(queens_quantity):
            for col in range(queens_quantity):
                color = self.color_2 if (row + col) % 2 == 0 else self.color_1
                pygame.draw.rect(self.screen, color, (col * self.tile_size, row * self.tile_size, self.tile_size,
                                                      self.tile_size))

                self.screen.blit(self.image, (solution[row] * self.tile_size, row * self.tile_size))


class QueenBoard:
    screen = None
    screen_size = None
    tile_size = None
    background = None
    running = True

    def __init__(self, size, tile_size, image, color_1, color_2):
        pygame.init()
        self.screen = pygame.display.set_mode(size)
        self.screen_size = self.screen.get_size()
        self.tile_size = tile_size
        self.background = BoardBackground(image, color_1, color_2, tile_size)
        pygame.display.set_caption('N Queens')

    def handle_events(self):
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                self.running = False

    def actors_draw(self, queens_quantity, solution):
        self.background.draw(queens_quantity, solution)

    def loop(self, queens_quantity, solution):
        while self.running:
            self.handle_events()

            self.actors_draw(queens_quantity, solution)

            pygame.display.flip()
