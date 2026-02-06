# Data Modeling Patterns

## Database Schema Design

### Naming Conventions

- Tables: plural, snake_case (`users`, `order_items`)
- Columns: snake_case (`user_id`, `created_at`)
- Primary keys: `id` (integer, auto-increment)
- Foreign keys: `<table>_id` (`user_id`, `order_id`)
- Timestamps: `created_at`, `updated_at`
- Soft deletes: `deleted_at`

### Users Table Example

```sql
CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  email VARCHAR(255) UNIQUE NOT NULL,
  password_hash VARCHAR(255) NOT NULL,
  name VARCHAR(100) NOT NULL,
  role VARCHAR(50) DEFAULT 'viewer',
  is_active BOOLEAN DEFAULT true,
  email_verified_at TIMESTAMP,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  deleted_at TIMESTAMP
);

-- Indexes
CREATE INDEX idx_users_email ON users(email);
CREATE INDEX idx_users_role ON users(role);
CREATE INDEX idx_users_created_at ON users(created_at);
```

## Relationships

### One-to-Many

```sql
-- Orders belong to users
CREATE TABLE orders (
  id SERIAL PRIMARY KEY,
  user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  status VARCHAR(50) DEFAULT 'pending',
  total DECIMAL(10, 2) NOT NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_orders_user_id ON orders(user_id);
CREATE INDEX idx_orders_status ON orders(status);
```

### Many-to-Many

```sql
-- Users can have many roles, roles can belong to many users
CREATE TABLE roles (
  id SERIAL PRIMARY KEY,
  name VARCHAR(50) UNIQUE NOT NULL,
  description TEXT
);

CREATE TABLE user_roles (
  user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  role_id INTEGER NOT NULL REFERENCES roles(id) ON DELETE CASCADE,
  granted_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (user_id, role_id)
);

CREATE INDEX idx_user_roles_user_id ON user_roles(user_id);
CREATE INDEX idx_user_roles_role_id ON user_roles(role_id);
```

### Self-Referencing

```sql
-- Users can follow other users
CREATE TABLE user_follows (
  follower_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  following_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (follower_id, following_id),
  CHECK (follower_id != following_id)
);
```

## Model Classes (ORM Pattern)

### Base Model

```javascript
class Model {
  static tableName = '';
  static timestamps = true;

  constructor(attributes = {}) {
    Object.assign(this, attributes);
  }

  static async find(id) {
    const row = await db(this.tableName).where({ id }).first();
    return row ? new this(row) : null;
  }

  static async findBy(criteria) {
    const row = await db(this.tableName).where(criteria).first();
    return row ? new this(row) : null;
  }

  static async all() {
    const rows = await db(this.tableName).select();
    return rows.map(row => new this(row));
  }

  async save() {
    if (this.id) {
      // Update
      if (this.constructor.timestamps) {
        this.updated_at = new Date();
      }
      await db(this.constructor.tableName)
        .where({ id: this.id })
        .update(this.toJSON());
    } else {
      // Create
      if (this.constructor.timestamps) {
        this.created_at = new Date();
        this.updated_at = new Date();
      }
      const [id] = await db(this.constructor.tableName)
        .insert(this.toJSON())
        .returning('id');
      this.id = id;
    }
    return this;
  }

  async delete() {
    await db(this.constructor.tableName).where({ id: this.id }).del();
  }

  toJSON() {
    return { ...this };
  }
}
```

### User Model

```javascript
class User extends Model {
  static tableName = 'users';

  // Relationships
  async orders() {
    const rows = await db('orders').where({ user_id: this.id });
    return rows.map(row => new Order(row));
  }

  async roles() {
    const rows = await db('roles')
      .join('user_roles', 'roles.id', 'user_roles.role_id')
      .where({ 'user_roles.user_id': this.id })
      .select('roles.*');
    return rows.map(row => new Role(row));
  }

  // Methods
  async hasRole(roleName) {
    const roles = await this.roles();
    return roles.some(role => role.name === roleName);
  }

  async isAdmin() {
    return await this.hasRole('admin');
  }

  // Validation
  validate() {
    const errors = [];

    if (!this.email || !isValidEmail(this.email)) {
      errors.push({ field: 'email', message: 'Valid email required' });
    }

    if (!this.name || this.name.length < 2) {
      errors.push({ field: 'name', message: 'Name must be at least 2 characters' });
    }

    return errors;
  }

  // Before save hook
  async save() {
    const errors = this.validate();
    if (errors.length > 0) {
      throw new ValidationError('Validation failed', errors);
    }

    return super.save();
  }

  // Serialize for API
  toJSON() {
    const { password_hash, ...safe } = this;
    return safe;  // Don't expose password hash
  }
}
```

## Migrations

### Creating Tables

```javascript
exports.up = function(knex) {
  return knex.schema.createTable('users', (table) => {
    table.increments('id').primary();
    table.string('email', 255).unique().notNullable();
    table.string('password_hash', 255).notNullable();
    table.string('name', 100).notNullable();
    table.string('role', 50).defaultTo('viewer');
    table.boolean('is_active').defaultTo(true);
    table.timestamp('email_verified_at');
    table.timestamps(true, true);  // created_at, updated_at
    table.timestamp('deleted_at');

    table.index('email');
    table.index('role');
  });
};

exports.down = function(knex) {
  return knex.schema.dropTable('users');
};
```

### Altering Tables

```javascript
exports.up = function(knex) {
  return knex.schema.table('users', (table) => {
    table.string('phone_number', 20);
    table.index('phone_number');
  });
};

exports.down = function(knex) {
  return knex.schema.table('users', (table) => {
    table.dropIndex('phone_number');
    table.dropColumn('phone_number');
  });
};
```

## Query Patterns

### Eager Loading (Avoid N+1)

```javascript
// BAD: N+1 query problem
const users = await User.all();
for (const user of users) {
  user.orders = await user.orders();  // N additional queries!
}

// GOOD: Join/eager load
const usersWithOrders = await db('users')
  .leftJoin('orders', 'users.id', 'orders.user_id')
  .select('users.*', db.raw('json_agg(orders) as orders'))
  .groupBy('users.id');
```

### Pagination

```javascript
async function getUsers(page = 1, limit = 20) {
  const offset = (page - 1) * limit;

  const [total, users] = await Promise.all([
    db('users').count('* as count').first(),
    db('users').select().limit(limit).offset(offset)
  ]);

  return {
    data: users,
    pagination: {
      page,
      limit,
      total: total.count,
      totalPages: Math.ceil(total.count / limit)
    }
  };
}
```

### Transactions

```javascript
async function transferFunds(fromUserId, toUserId, amount) {
  return await db.transaction(async (trx) => {
    // Deduct from sender
    await trx('accounts')
      .where({ user_id: fromUserId })
      .decrement('balance', amount);

    // Add to receiver
    await trx('accounts')
      .where({ user_id: toUserId })
      .increment('balance', amount);

    // Record transaction
    await trx('transactions').insert({
      from_user_id: fromUserId,
      to_user_id: toUserId,
      amount
    });

    // If any query fails, all changes are rolled back
  });
}
```

## Soft Deletes

```javascript
class SoftDeleteModel extends Model {
  async delete() {
    this.deleted_at = new Date();
    await this.save();
  }

  static async find(id) {
    const row = await db(this.tableName)
      .where({ id })
      .whereNull('deleted_at')
      .first();
    return row ? new this(row) : null;
  }

  static async withTrashed() {
    const rows = await db(this.tableName).select();
    return rows.map(row => new this(row));
  }

  async restore() {
    this.deleted_at = null;
    await this.save();
  }
}
```

## Data Validation

### At Model Level

```javascript
class Product extends Model {
  validate() {
    const errors = [];

    if (!this.name || this.name.length < 3) {
      errors.push({ field: 'name', message: 'Name must be at least 3 characters' });
    }

    if (this.price <= 0) {
      errors.push({ field: 'price', message: 'Price must be positive' });
    }

    if (this.stock < 0) {
      errors.push({ field: 'stock', message: 'Stock cannot be negative' });
    }

    if (errors.length > 0) {
      throw new ValidationError('Validation failed', errors);
    }
  }
}
```

### At Database Level

```sql
CREATE TABLE products (
  id SERIAL PRIMARY KEY,
  name VARCHAR(100) NOT NULL CHECK (length(name) >= 3),
  price DECIMAL(10, 2) NOT NULL CHECK (price > 0),
  stock INTEGER NOT NULL DEFAULT 0 CHECK (stock >= 0),
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

---

**Key Principles**:
1. Normalize to reduce redundancy
2. Index foreign keys and frequently queried columns
3. Use transactions for multi-step operations
4. Validate at both application and database levels
5. Plan for data growth (pagination, archiving)
