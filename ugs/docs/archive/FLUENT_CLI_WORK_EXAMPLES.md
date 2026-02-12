# Fluent CLI for Current Enhancement Work

## Your Enhancement Tasks with Natural Commands:

```bash
# Start adjacency lists work:
ugs project:ugs update adjacency_lists set status=in_progress,assignee=bln,start_date=now()

# Property indexing progress:
ugs project:ugs update property_indexing set status=in_progress,progress=15,notes="Designing index structure"

# Query current work:
ugs project:ugs select name,status,priority from tasks where assignee=bln and status in (todo,in_progress)

# Update dependencies when adjacency lists complete:
ugs project:ugs update (select tasks where depends_on contains adjacency_lists) set status=ready

# Bulk status updates:
ugs project:ugs update tasks set last_reviewed=now() where priority=high
```

## Natural Daily Workflow:

```bash
# Daily standup view:
ugs project:ugs select name,status,priority from tasks where assignee=bln

# Personal research that feeds into work:
ugs user:bln select title from ideas where status=researching
ugs link user:bln.vector_embeddings to project:ugs.full_text_search as extends
```

This makes UGS feel like a natural workspace rather than a database tool!
