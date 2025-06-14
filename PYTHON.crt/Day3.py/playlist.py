'''7. write a python program to:
add 10 songs to a playlist.
show the playlist in normal and reverse order.'''

List=['sa','re','ga','ma','pa','da','ne','sa','va','li']
print(List)
reverse=List[::-1]
print(reverse) 




playlist = []
# Add 10 songs to the playlist
for i in range(1, 11):
    song = input(f"Enter the name of song {i}: ")
    playlist.append(song)

# Display the playlist in normal order
print("\nPlaylist in normal order:")
for song in playlist:
    print(song)

# Display the playlist in reverse order
print("\nPlaylist in reverse order:")
for song in reversed(playlist):
    print(song)  




'''
8. write a python program to:
input a lsit of numbers
create two new lists: one for even numbers , one for odd numbers.
display both lists '''    

'''numbers = list(map(int, input("Enter numbers separated by space: ").split()))
even_numbers = []
odd_numbers = []
for num in numbers:
    if num % 2 == 0:
        even_numbers.append(num)
    else:
        odd_numbers.append(num)
print("Even numbers:", even_numbers)
print("Odd numbers:", odd_numbers) '''



