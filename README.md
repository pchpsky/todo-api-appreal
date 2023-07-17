# Todo API

The Todo API is a RESTful web service built with Cowboy

## Features

- Create, read, update, and delete todos.

## Getting Started

To get started with the Todo API, follow the steps below:

1. Clone the repository:
```bash
git clone https://github.com/pchpsky/todo-api-appreal.git
```

2. Make sure you have Erlang installed on your system.

3. Install dependencies:
```bash
cd todo-api-appreal
rebar3 deps
```

4. Start the server:
```bash
rebar3 shell
```

5. The Todo API is now running and can be accessed at `http://localhost:8080`.

## API Endpoints

The following API endpoints are available:

- `GET /todos`: Retrieves a list of all todos.
```bash
curl -X 'GET' \
  'http://localhost:8080/todos' \
  -H 'accept: application/json'
```

- `GET /todos/:id`: Retrieves a specific todo by its ID.
```bash
curl -X 'GET' \
  'http://localhost:8080/todos/1' \
  -H 'accept: application/json'
```

- `POST /todos`: Creates a new todo.
```bash
curl -X 'POST' \
  'http://localhost:8080/todos' \
  -H 'accept: application/json' \
  -H 'Content-Type: application/json' \
  -d '{
  "title": "Test Todo"
}'
```

- `PUT /todos/:id`: Updates an existing todo.
```bash
curl -X 'PUT' \
  'http://localhost:8080/todos/1' \
  -H 'accept: application/json' \
  -H 'Content-Type: application/json' \
  -d '{
  "title": "New Title",
  "completed": true
}'
```

- `DELETE /todos/:id`: Deletes a specific todo by its ID.
```bash
curl -X 'DELETE' \
  'http://localhost:8080/todos/1' \
  -H 'accept: application/json'
```

- `POST /todos/:id/complete`: Marks a specific todo as completed.
```bash
curl -X 'POST' \
  'http://localhost:8080/todos/1/complete' \
  -H 'Content-Type: application/json' \
  -H 'accept: application/json' \
  -d ''
```

For detailed information on request and response formats, please refer to the API documentation at `http://localhost:8080/swagger-ui`.


## Testing

To run the test suite, use the following command:

```bash
rebar3 eunit
```
