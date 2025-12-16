# load packages
import matplotlib.pyplot as plt
import numpy as np
from PIL import Image 

# define function for adding icons
# from https://python-graph-gallery.com/web-stacked-line-chart-with-labels/
def add_logo(path_to_logo, text, image_bottom_left_x, image_bottom_left_y, image_width, x_offset, y_offset):
    """
    Adds a logo image and text to a plot at specific positions.

    Parameters:
        path_to_logo (str): The file path to the logo image that will be added to the plot.
        text (str): The text to be added along with the logo.
        image_bottom_left_x (float): The x-coordinate of the bottom-left corner of the logo image's position on the plot.
        image_bottom_left_y (float): The y-coordinate of the bottom-left corner of the logo image's position on the plot.
        image_width (float): The width of the logo image in the plot.
    """
    logo = Image.open(path_to_logo) # Open the image
    image_array = np.array(logo) # Convert to a numpy array
    image_height = image_width * image_array.shape[0] / image_array.shape[1]  # Calculate height based on ratio

    # Add image to graph 
    ax_image = plt.axes([image_bottom_left_x, # Position on the x-axis
                         image_bottom_left_y, # Position on the y-axis
                         image_width, # Image width
                         image_height]) # Image height
    ax_image.imshow(image_array) # Display the image
    ax_image.axis('off') # Remove axis of the image in order to improve style
    
    # Add text
    plt.text(x_offset, # Position on the x-axis
             image_bottom_left_y+y_offset, # Position on the y-axis
             text,
             fontsize=5)
