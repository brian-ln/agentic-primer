# Naming Conventions

Clear, consistent naming makes code self-documenting and reduces cognitive load.

## Core Principles

1. **Clarity**: Names should reveal intent
2. **Consistency**: Use established patterns
3. **Searchability**: Avoid single-letter names (except loop counters)
4. **Pronounceable**: Should be easy to say aloud in discussions

## Variables

### Booleans

Prefix with `is`, `has`, `should`, or `can`:

```javascript
// GOOD
const isActive = true;
const hasPermission = checkPermission(user);
const shouldRetry = errorCount < MAX_RETRIES;
const canEdit = user.role === 'admin';

// BAD
const active = true;  // Ambiguous: status or action?
const permission = checkPermission(user);  // What type is this?
```

### Numbers

Use descriptive names that include units when relevant:

```javascript
// GOOD
const maxRetries = 3;
const timeoutMs = 5000;
const cacheDurationMinutes = 15;
const fileSizeBytes = 1024 * 1024;

// BAD
const max = 3;  // Max what?
const timeout = 5000;  // Seconds or milliseconds?
const duration = 15;  // Minutes, hours, days?
```

### Strings

Be specific about content:

```javascript
// GOOD
const userName = 'john_doe';
const emailAddress = 'john@example.com';
const apiEndpoint = '/api/v1/users';
const errorMessage = 'Invalid credentials';

// BAD
const name = 'john_doe';  // First name, last name, username?
const email = 'john@example.com';  // OK, but emailAddress is clearer
const url = '/api/v1/users';  // Full URL or path?
```

### Arrays and Collections

Use plural names:

```javascript
// GOOD
const users = await db.users.findAll();
const activeOrders = orders.filter(o => o.status === 'active');
const emailAddresses = users.map(u => u.email);

// BAD
const userList = await db.users.findAll();  // Redundant 'List'
const activeOrder = orders.filter(...);  // Singular for collection
```

### Objects and Maps

Use singular for single entities, descriptive names for maps:

```javascript
// GOOD
const user = { id: 1, name: 'John' };
const userById = new Map();  // Map<ID, User>
const errorsByCode = {};  // Object with error codes as keys

// BAD
const users = { id: 1, name: 'John' };  // Plural for single object
const map = new Map();  // What does it map?
```

## Functions

### Action Verbs

Start with verbs that describe the action:

```javascript
// GOOD
function getUser(id) { }
function createOrder(data) { }
function updateProfile(userId, changes) { }
function deleteAccount(userId) { }
function validateEmail(email) { }
function calculateTotal(items) { }
function sendNotification(user, message) { }

// BAD
function user(id) { }  // Noun, not verb
function order(data) { }  // Ambiguous: get or create?
```

### Common Prefixes

| Prefix | Meaning | Example |
|--------|---------|---------|
| `get` | Retrieve data | `getUserById(id)` |
| `fetch` | Retrieve from external source | `fetchOrdersFromAPI()` |
| `find` | Search for data | `findUserByEmail(email)` |
| `create` | Create new entity | `createAccount(data)` |
| `update` | Modify existing | `updatePassword(userId, newPassword)` |
| `delete` | Remove entity | `deleteComment(commentId)` |
| `remove` | Remove from collection | `removeItemFromCart(itemId)` |
| `is/has/can` | Return boolean | `isAdmin(user)`, `hasPermission()` |
| `validate` | Check validity | `validateInput(data)` |
| `calculate` | Compute value | `calculateTax(amount)` |
| `send` | Transmit data | `sendEmail(to, subject, body)` |
| `handle` | Event handler | `handleClick(event)` |
| `on` | Event listener | `onClick()`, `onSubmit()` |

### Predicates (Boolean-Returning Functions)

```javascript
// GOOD
function isValidEmail(email) {
  return /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email);
}

function hasActiveSubscription(user) {
  return user.subscription && user.subscription.status === 'active';
}

function canAccessResource(user, resource) {
  return user.permissions.includes(resource.requiredPermission);
}

// BAD
function checkEmail(email) {
  return /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email);  // 'check' is ambiguous
}
```

### Async Functions

No special naming required, but consider context:

```javascript
// GOOD: Clear from context
async function fetchUser(id) {
  return await db.users.findById(id);
}

// ACCEPTABLE: If clarity helps
async function fetchUserAsync(id) {
  return await db.users.findById(id);
}
```

## Classes

### Class Names

Use nouns in PascalCase:

```javascript
// GOOD
class UserService { }
class OrderController { }
class PaymentProcessor { }
class EmailValidator { }
class DatabaseConnection { }

// BAD
class UserManager { }  // Too generic; prefer specific role (Service, Controller)
class HandleOrders { }  // Verb phrase; should be noun
class user_service { }  // Wrong case
```

### Method Names

Same as functions, but consider object context:

```javascript
class UserService {
  // GOOD: Context is clear from class name
  getById(id) { }
  create(data) { }
  update(id, changes) { }
  delete(id) { }

  // ALSO GOOD: Explicit if exported/public
  getUserById(id) { }
  createUser(data) { }

  // Private methods: prefix with underscore
  _validateUserData(data) { }
  _hashPassword(password) { }
}
```

## Files and Directories

### Files

Use kebab-case for files:

```
user-service.js          // Service class
auth-middleware.js       // Middleware
user.routes.js          // Routes
user.model.js           // Model
user.test.js            // Tests
user.utils.js           // Utilities
```

### Directories

Use kebab-case for directories:

```
src/
  controllers/
  services/
  models/
  middleware/
  utils/
  routes/
tests/
  unit/
  integration/
  e2e/
```

## Constants and Enums

### Constants

Use UPPER_SNAKE_CASE:

```javascript
// GOOD
const MAX_RETRY_ATTEMPTS = 3;
const DEFAULT_TIMEOUT_MS = 5000;
const API_BASE_URL = 'https://api.example.com';
const HTTP_STATUS_OK = 200;

// BAD
const maxRetryAttempts = 3;  // Should be uppercase
const max_retry = 3;  // Incomplete name
```

### Enums (Object as Enum)

```javascript
// GOOD
const OrderStatus = {
  PENDING: 'pending',
  PROCESSING: 'processing',
  SHIPPED: 'shipped',
  DELIVERED: 'delivered',
  CANCELLED: 'cancelled'
};

const UserRole = {
  ADMIN: 'admin',
  EDITOR: 'editor',
  VIEWER: 'viewer'
};

// Usage
if (order.status === OrderStatus.PENDING) { }
```

## API Endpoints

### REST Endpoints

```javascript
// GOOD: Resource-oriented
GET    /api/users              // List users
GET    /api/users/:id          // Get single user
POST   /api/users              // Create user
PUT    /api/users/:id          // Update user
PATCH  /api/users/:id          // Partial update
DELETE /api/users/:id          // Delete user

// Nested resources
GET    /api/users/:id/orders   // User's orders
POST   /api/orders/:id/items   // Add item to order

// BAD
GET    /api/getUsers            // RPC-style, not RESTful
POST   /api/createUser          // Redundant verb
GET    /api/user/:id            // Inconsistent (singular vs plural)
```

### Route Handler Names

```javascript
// GOOD
router.get('/users', listUsers);
router.get('/users/:id', getUserById);
router.post('/users', createUser);
router.put('/users/:id', updateUser);
router.delete('/users/:id', deleteUser);

// BAD
router.get('/users', users);  // Ambiguous
router.post('/users', handler);  // Non-descriptive
```

## Database

### Table Names

Plural, snake_case:

```sql
-- GOOD
users
orders
order_items
user_preferences

-- BAD
user          -- Singular
Users         -- PascalCase
order-items   -- Kebab case
```

### Column Names

snake_case:

```sql
-- GOOD
id
user_id
email_address
created_at
is_active

-- BAD
userId        -- camelCase
EmailAddress  -- PascalCase
createdAt     -- camelCase
```

### Indexes

Prefix with `idx_`:

```sql
-- GOOD
CREATE INDEX idx_users_email ON users(email);
CREATE INDEX idx_orders_user_id ON orders(user_id);

-- BAD
CREATE INDEX users_email ON users(email);  -- No prefix
CREATE INDEX email_index ON users(email);  -- Inconsistent naming
```

## Events

### Event Names

Use past tense, dot notation for namespacing:

```javascript
// GOOD
eventEmitter.emit('user.created', user);
eventEmitter.emit('order.shipped', order);
eventEmitter.emit('payment.failed', { orderId, error });

// Event listeners
eventEmitter.on('user.created', sendWelcomeEmail);
eventEmitter.on('order.shipped', sendShippingNotification);

// BAD
eventEmitter.emit('createUser', user);  // Present tense
eventEmitter.emit('USER_CREATED', user);  // Screaming case
```

## Environment Variables

Use UPPER_SNAKE_CASE:

```bash
# GOOD
DATABASE_URL=postgres://localhost/mydb
API_KEY=abc123
NODE_ENV=production
MAX_CONNECTIONS=10
ENABLE_LOGGING=true

# BAD
databaseUrl=postgres://localhost/mydb  # Wrong case
api-key=abc123  # Kebab case
```

## Abbreviations

### When to Abbreviate

Generally avoid abbreviations unless widely understood:

```javascript
// GOOD: Common abbreviations
const id = user.id;
const url = 'https://example.com';
const api = new APIClient();
const html = renderHTML();
const json = JSON.stringify(data);
const db = connectDatabase();

// GOOD: Full words for clarity
const configuration = loadConfig();  // Not cfg
const authentication = setupAuth();  // Not auth (unless very common)
const repository = new Repository();  // Not repo

// Context matters
const auth = setupAuthentication();  // OK: auth is industry-standard
const repo = git.getRepository();    // OK: repo is widely understood
```

## Naming Anti-Patterns

### Avoid

```javascript
// Generic names
const data = fetchData();  // What kind of data?
const temp = processTemp();  // Temporary what?
const result = doSomething();  // What result?

// Hungarian notation (encoding type in name)
const strName = 'John';  // Type is obvious in modern IDEs
const arrUsers = [];  // Redundant
const objConfig = {};  // Obvious from value

// Encoded information
const user1 = getUser(1);  // Use descriptive name
const user2 = getUser(2);
// Better:
const currentUser = getUser(currentUserId);
const targetUser = getUser(targetUserId);

// Misleading names
function getUser(id) {
  // BAD: Actually creates user if doesn't exist!
  return db.users.findOrCreate({ id });
}

// Redundant naming
const userUser = fetchUser();  // Repeats context
const userObject = { };  // 'Object' is redundant
```

## Context-Aware Naming

Leverage surrounding context:

```javascript
// GOOD: Context from class/module
class UserService {
  getById(id) { }     // Clear: UserService.getById
  create(data) { }    // Clear: UserService.create
}

// GOOD: Context from nesting
const user = {
  profile: {
    firstName: 'John',  // user.profile.firstName
    lastName: 'Doe'
  }
};

// BAD: Redundant with context
class UserService {
  getUserById(id) { }  // Redundant: UserService.getUserById
  createUser(data) { }
}
```

---

**Key Takeaway**: Choose names that make code read like well-written prose. If you hesitate when reading a name, it probably needs improvement.
