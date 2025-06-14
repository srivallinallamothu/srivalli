# Program to read and print details of n students

n = int(input("Enter number of students: "))
students = []
for i in range(n):
    print(f"\nEnter details for Student {i+1}:")
    name = input("Enter Student Name: ")
    student_id = input("Enter Student ID: ")
    percentage = float(input("Enter Percentage: "))
    branch = input("Enter Branch: ")
    students.append({
        'Name': name,
        'ID': student_id,
        'Percentage': percentage,
        'Branch': branch
    })
print("\n--- Students Details ---")
for i, student in enumerate(students, start=1):
    print(f"\nStudent {i}:")
    print(f"Name: {student['Name']}")
    print(f"ID: {student['ID']}")
    print(f"Percentage: {student['Percentage']}")
    print(f"Branch: {student['Branch']}")
