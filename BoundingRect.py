from PIL import Image

def find_largest_rectangle(image):
    width, height = image.size
    pic = image.load()

    max_area = 0
    max_rect = (0, 0, 0, 0)

    for x in range(width):
        tow = [0]*height
        right = [width-1]*height
        left = [0]*height
        if pic[x,0][3] > 0:
            tow[0] = 1
            left[0]=0
            while(left[0] < width-1 and pic[left[0],0][3] == 0):
                left[0]+=1
            right[0]=width-1
            while(right[0] > 0 and pic[right[0],0][3] == 0):
                right[0]-=1
        for y in range(1,height):
            if pic[x,y][3] > 0:
                tow[y] = tow[y-1] + 1
                left[y] = left[y-1]
                if pic[left[y],y][3] == 0:
                    while(pic[left[y],y][3] == 0):
                        left[y] += 1
                right[y] = right[y-1]
                if pic[right[y],y][3] == 0:
                    while(pic[right[y],y][3] == 0):
                        right[y] -= 1

                my_area = tow[y] * (right[y] - left[y])
                if my_area > max_area:
                    max_area = my_area
                    max_rect = (left[y],y-tow[y],right[y]-left[y],tow[y])

    return (max_rect, max_area, image.size)

def print_torus(max_rect, image):
    x,y,w,h = max_rect
    
    new_image = Image.new(mode=image.mode, size=(2*w,2*h))
    new_image.paste(image, (0*w - x, 0*h - y), mask=image)
    new_image.paste(image, (0*w - x, 1*h - y), mask=image)
    new_image.paste(image, (0*w - x, 2*h - y), mask=image)
    new_image.paste(image, (1*w - x, 0*h - y), mask=image)
    new_image.paste(image, (1*w - x, 1*h - y), mask=image)
    new_image.paste(image, (1*w - x, 2*h - y), mask=image)
    new_image.paste(image, (2*w - x, 0*h - y), mask=image)
    new_image.paste(image, (2*w - x, 1*h - y), mask=image)
    new_image.paste(image, (2*w - x, 2*h - y), mask=image)
    final_image = new_image.crop((w/2, h/2, w+w/2, h+h/2))
    final_image.show()
    final_image.save("/home/<USER>/ProjectFolder/result.png")
    

image = Image.open(r"/home/<USER>/ProjectFolder/source.png")
rectangle = find_largest_rectangle(image)
print_torus(rectangle[0], image)
print("Rectangle:",rectangle[0])
print("Area:",rectangle[1])
print("Original:",rectangle[2][0],"x",rectangle[2][1])
