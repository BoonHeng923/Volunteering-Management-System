# Volunteering Management System

## Overview
The **Volunteering Management System** is a Haskell-based console application designed to streamline volunteer data management through the application of functional programming principles. It allows users to add, delete, search, edit, display, sort, filter and generate reports for volunteers in a structured, efficient, purely functional way. This system leverages Haskell's strong type system, pure functions, and monadic state management to ensure data integrity, composability, and ease of maintenance.

## Key Features
- **Add Volunteer:** Add new volunteer information with full validation.
- **Delete Volunteer:** Remove a volunteer by ID after confirmation.
- **Search Volunteer:** Search and display details for a specific volunteer.
- **Edit Volunteer:** Modify specific volunteer fields safely with re-validation.
- **Display All Volunteers:** View all volunteers in a neatly formatted table.
- **Sort Volunteers:** Sort data by ID, name, contact, hours contributed, or number of events participated (ascending/descending).
- **Filter Volunteers:** Filter by role or availability.
- **Generate Reports:** View insightful volunteer statistics.

## Report Features
- **Role Report:** Counts volunteers by role (Community, Environment, Health, Wildlife).
- **Availability Report:** Summarizes how many volunteers are Available and Unavailable.
- **Top Volunteer Report:** Displays top 3 volunteers by hours contributed.
- **Status Report:** Categorizes volunteers as active or inactive based on participation.
- **Participation Report:** Displays total volunteers, total hours, and total number of events participated.

All reports are composable using Monoid and Semigroup instances, allowing users to merge multiple reports together.

## Key Functional Programming Concepts Applied
- **Pure Functions:**  Deterministic functions without side effects (validateID, validateName, etc.).
- **Higher-Order Functions:** Use of map, filter, and sortOn for list operations.
- **Type Abstraction & Type Classes:** Custom types like Volunteer, Role, Availability, and the MenuOption type class.
- **Functor, Applicative & Monad Instances:** Implemented in the custom Validation type for composable input validation.
- **State Monad:** Used to manage volunteer data (addVolunteerState, deleteVolunteerState, editVolunteerState) while maintaining immutability.
- **Semigroup & Monoid Instances:** Enable flexible and compositional report generation.
- **Pattern Matching:** Simplifies logic in validation, menus, CSV parsing, and error handling.
- **Immutability:** All state changes return new states instead of mutating existing data.

## File Operations
All data is stored in a CSV file, ensuring persistence across sessions.
- **Read & Parse:** readVolunteerFromCSV and parseCSVLine.
- **Write & Save:** saveVolunteerToCSV (uses a temporary file for safe updates).
- **CSV Header:** Defined for consistency across all file operations.
Error handling: Implemented using the catch function and safe resource management (withFile).

## User Experience
- Color-coded outputs for better readability:
  1. **Yellow:** Welcome and exit messages with ASCII Art.
  2. **Cyan:** Section header.
  3. **Green:** Success messages.
  4. **Red:** Error messages.
 
## Validation Features:
Every user input is validated before processing:
- **Volunteer ID:** Must be numeric, 5 digits long.
- **Name:** Must contain only letters and spaces, converted to Title Case.
- **Contact:** Must start with 01 and be 10–11 digits long.
- **Role:** Must be one of the predefined roles.
- **Availability:** Must be "Available" or "Unavailable".
- **Hours & Number of Events Participated:** Must be positive numbers.
  
Each invalid input triggers a descriptive error message and re-prompt.

## Credit
**Developer:** Ong Boon Heng <br>
**Course:** Bachelor of Science (Honours) in Computer Science <br>
**Institution:** Faculty of Engineering and Technology, Sunway University
