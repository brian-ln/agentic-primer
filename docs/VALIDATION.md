# System Validation

This document describes how to validate that the automation system is working correctly.

## Pre-Deployment Checklist

Before using the system, verify:

- [ ] All workflow files are in `.github/workflows/`
- [ ] All issue templates are in `.github/ISSUE_TEMPLATE/`
- [ ] Knowledge base directories exist in `docs/knowledge/`
- [ ] README.md is present and complete
- [ ] CONTRIBUTING.md is present and complete

## Validation Steps

### 1. Verify File Structure

```bash
# Check workflows exist
ls -la .github/workflows/
# Should show: issue-router.yml, research-automation.yml, 
#              planning-automation.yml, implementation-automation.yml

# Check templates exist
ls -la .github/ISSUE_TEMPLATE/
# Should show: config.yml, research.yml, planning.yml, implementation.yml

# Check knowledge base
ls -la docs/knowledge/
# Should show: research/, planning/, implementation/, README.md
```

### 2. Validate YAML Syntax

```bash
# If yamllint is available
yamllint -d relaxed .github/

# Or use Python
python3 -c "import yaml; yaml.safe_load(open('.github/workflows/issue-router.yml'))"
```

### 3. Test Issue Template Visibility

1. Go to repository → Issues → New Issue
2. Verify all templates are visible:
   - 🔬 Research Task
   - 📋 Planning Task
   - 🔨 Implementation Task

### 4. Test Research Workflow

1. Create a test research issue:
   - Title: "[Research] Test automation"
   - Use research template
   - Fill required fields
   - Submit

2. Expected results:
   - ✅ Issue router comments on the issue
   - ✅ Research automation workflow runs
   - ✅ Branch `research/issue-{number}` created
   - ✅ PR opened with research document
   - ✅ Document exists at `docs/knowledge/research/issue-{number}.md`

3. Check GitHub Actions tab for workflow status

### 5. Test Planning Workflow

1. Create a test planning issue:
   - Title: "[Planning] Test automation"
   - Use planning template
   - Fill required fields
   - Submit

2. Expected results:
   - ✅ Issue router comments on the issue
   - ✅ Planning automation workflow runs
   - ✅ Branch `planning/issue-{number}` created
   - ✅ PR opened with planning document
   - ✅ Document exists at `docs/knowledge/planning/issue-{number}.md`

3. Check GitHub Actions tab for workflow status

### 6. Test Implementation Workflow

1. Create a test implementation issue:
   - Title: "[Implementation] Test automation"
   - Use implementation template
   - Fill required fields
   - Submit

2. Expected results:
   - ✅ Issue router comments on the issue
   - ✅ Implementation automation workflow runs
   - ✅ Branch `implementation/issue-{number}` created
   - ✅ PR opened with tracking document
   - ✅ Document exists at `docs/knowledge/implementation/issue-{number}.md`

3. Check GitHub Actions tab for workflow status

## Troubleshooting

### Workflow Doesn't Run

**Possible causes:**
- Incorrect label (must be exact: `research`, `planning`, or `implementation`)
- Workflow file syntax error
- Missing repository permissions

**Solutions:**
1. Check issue has correct label
2. Validate YAML syntax
3. Verify Actions are enabled in repository settings
4. Check workflow file permissions

### Branch Not Created

**Possible causes:**
- Branch already exists
- Insufficient permissions
- Workflow failed before branch creation

**Solutions:**
1. Delete existing branch if present
2. Check repository permissions
3. Review workflow logs in Actions tab

### PR Not Created

**Possible causes:**
- Branch creation failed
- No changes to commit
- Insufficient permissions

**Solutions:**
1. Verify branch exists
2. Check workflow logs
3. Verify PR creation permissions

### Document Not Generated

**Possible causes:**
- Directory doesn't exist
- File write error
- Template syntax error

**Solutions:**
1. Verify `docs/knowledge/{type}/` exists
2. Check workflow logs for errors
3. Validate heredoc syntax in workflow

## Common Workflow Errors

### Error: "reference already exists"

**Cause:** Branch already exists from previous run

**Solution:**
```bash
git push origin --delete research/issue-5  # or planning/issue-X, etc.
```

### Error: "refusing to allow a GitHub App to create or update workflow"

**Cause:** Workflow trying to modify another workflow

**Solution:** Ensure workflows only modify files in `docs/`, not `.github/`

### Error: "Resource not accessible by integration"

**Cause:** Missing permissions

**Solution:** Verify workflow has required permissions:
```yaml
permissions:
  issues: write
  contents: write
  pull-requests: write
```

## Performance Metrics

Track these metrics to ensure system health:

- **Workflow execution time** - Should be < 60 seconds
- **Success rate** - Should be > 95%
- **PR creation rate** - Should match issue creation rate
- **Document quality** - Review generated documents periodically

## Maintenance

### Weekly
- Review open PRs from automation
- Check for failed workflows
- Monitor disk space (knowledge base growth)

### Monthly
- Review and archive old branches
- Update workflow actions to latest versions
- Review and update document templates
- Collect feedback from users

### Quarterly
- Analyze workflow metrics
- Identify improvement opportunities
- Update documentation
- Add new features based on usage

## Success Criteria

The system is working correctly when:

✅ Issues with labels trigger appropriate workflows  
✅ Workflows complete successfully within 60 seconds  
✅ Branches are created automatically  
✅ Documents are generated correctly  
✅ PRs are opened with proper content  
✅ Issue comments provide clear status  
✅ Knowledge base grows with each issue  

## Next Steps After Validation

1. Close and clean up test issues/PRs
2. Create documentation for your team
3. Train team members on the system
4. Start using it for real work
5. Gather feedback and iterate

## Support

If validation fails:
1. Check workflow logs in Actions tab
2. Review this troubleshooting guide
3. Check CONTRIBUTING.md for detailed workflow info
4. Review ARCHITECTURE.md for technical details
5. Open an issue for help
