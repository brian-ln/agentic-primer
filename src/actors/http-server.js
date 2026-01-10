/**
 * HTTPServerActor - HTTP REST API server for event system
 *
 * Responsibilities:
 * - Serve REST API endpoints for event system operations
 * - POST /events - Emit new events
 * - GET /events - Query events with filters
 * - GET /functions - List registered functions
 * - GET /patterns - List registered patterns
 * - GET /health - Health check endpoint
 * - Parse JSON request bodies
 * - Return proper HTTP status codes
 * - Handle CORS for browser access
 */

import { PROTOCOLS, ACTIONS, createMessage } from '../protocol.js';

/**
 * HTTPServerActor class
 */
export class HTTPServerActor {
  constructor(config = {}) {
    this.port = config.http?.port || 3000;
    this.host = config.http?.host || 'localhost';
    this.server = null;
    this.isRunning = false;
    this.startTime = null;

    // References to other actors (will be injected)
    this.eventLog = null;
    this.functionRegistry = null;
    this.patternMatcher = null;
  }

  /**
   * Set actor references for handling requests
   */
  setActors({ eventLog, functionRegistry, patternMatcher }) {
    this.eventLog = eventLog;
    this.functionRegistry = functionRegistry;
    this.patternMatcher = patternMatcher;
  }

  /**
   * Initialize and start the HTTP server
   */
  async initialize() {
    if (this.isRunning) {
      return {
        success: false,
        error: 'Server is already running'
      };
    }

    try {
      this.server = Bun.serve({
        port: this.port,
        hostname: this.host,
        fetch: async (request) => {
          return await this.handleRequest(request);
        },
        error: (error) => {
          console.error('[HTTPServerActor] Server error:', error);
          return new Response('Internal Server Error', { status: 500 });
        }
      });

      this.isRunning = true;
      this.startTime = new Date().toISOString();

      console.log(`[HTTPServerActor] Server listening on http://${this.host}:${this.port}`);

      return {
        success: true,
        url: `http://${this.host}:${this.port}`,
        port: this.port
      };
    } catch (error) {
      return {
        success: false,
        error: `Failed to start server: ${error.message}`
      };
    }
  }

  /**
   * Handle incoming HTTP requests
   */
  async handleRequest(request) {
    const url = new URL(request.url);
    const method = request.method;
    const path = url.pathname;

    // Add CORS headers for browser access
    const corsHeaders = {
      'Access-Control-Allow-Origin': '*',
      'Access-Control-Allow-Methods': 'GET, POST, OPTIONS',
      'Access-Control-Allow-Headers': 'Content-Type',
    };

    // Handle preflight requests
    if (method === 'OPTIONS') {
      return new Response(null, {
        status: 204,
        headers: corsHeaders
      });
    }

    try {
      // Route to appropriate handler
      if (method === 'POST' && path === '/events') {
        return await this.handlePostEvent(request, corsHeaders);
      }

      if (method === 'GET' && path === '/events') {
        return await this.handleGetEvents(request, corsHeaders);
      }

      if (method === 'GET' && path === '/functions') {
        return await this.handleGetFunctions(request, corsHeaders);
      }

      if (method === 'GET' && path === '/patterns') {
        return await this.handleGetPatterns(request, corsHeaders);
      }

      if (method === 'GET' && path === '/health') {
        return await this.handleGetHealth(request, corsHeaders);
      }

      // 404 for unknown routes
      return this.jsonResponse(
        { error: 'Not Found', path, method },
        404,
        corsHeaders
      );
    } catch (error) {
      console.error('[HTTPServerActor] Request error:', error);
      return this.jsonResponse(
        { error: 'Internal Server Error', message: error.message },
        500,
        corsHeaders
      );
    }
  }

  /**
   * POST /events - Emit a new event
   */
  async handlePostEvent(request, corsHeaders) {
    try {
      // Parse JSON body
      const body = await request.json();

      if (!body || typeof body !== 'object') {
        return this.jsonResponse(
          { error: 'Request body must be a JSON object' },
          400,
          corsHeaders
        );
      }

      // Validate event structure
      if (!body.type) {
        return this.jsonResponse(
          { error: 'Event type is required' },
          400,
          corsHeaders
        );
      }

      // Check if EventLogActor is available
      if (!this.eventLog) {
        return this.jsonResponse(
          { error: 'Event log not available' },
          503,
          corsHeaders
        );
      }

      // Send event to EventLogActor
      const result = await this.eventLog.handleMessage(
        createMessage(PROTOCOLS.EVENT, ACTIONS.APPEND, body)
      );

      if (result.success) {
        return this.jsonResponse(
          {
            success: true,
            eventId: result.eventId,
            eventCount: result.eventCount
          },
          201,
          corsHeaders
        );
      } else {
        return this.jsonResponse(
          { error: result.error },
          400,
          corsHeaders
        );
      }
    } catch (error) {
      if (error.name === 'SyntaxError') {
        return this.jsonResponse(
          { error: 'Invalid JSON in request body' },
          400,
          corsHeaders
        );
      }
      throw error;
    }
  }

  /**
   * GET /events - Query events
   */
  async handleGetEvents(request, corsHeaders) {
    try {
      const url = new URL(request.url);
      const params = url.searchParams;

      // Check if EventLogActor is available
      if (!this.eventLog) {
        return this.jsonResponse(
          { error: 'Event log not available' },
          503,
          corsHeaders
        );
      }

      // Build query options from URL parameters
      const options = {
        limit: params.has('limit') ? parseInt(params.get('limit'), 10) : 100,
        offset: params.has('offset') ? parseInt(params.get('offset'), 10) : 0,
        reverse: params.has('reverse') ? params.get('reverse') === 'true' : false
      };

      // Add filter if type parameter is provided
      if (params.has('type')) {
        const eventType = params.get('type');
        options.filter = (event) => event.type === eventType;
      }

      // Query events
      const result = await this.eventLog.handleMessage(
        createMessage(PROTOCOLS.EVENT, ACTIONS.QUERY, options)
      );

      if (result.success) {
        return this.jsonResponse(
          {
            success: true,
            events: result.events,
            count: result.count,
            total: result.total
          },
          200,
          corsHeaders
        );
      } else {
        return this.jsonResponse(
          { error: result.error },
          500,
          corsHeaders
        );
      }
    } catch (error) {
      throw error;
    }
  }

  /**
   * GET /functions - List registered functions
   */
  async handleGetFunctions(request, corsHeaders) {
    try {
      const url = new URL(request.url);
      const params = url.searchParams;

      // Check if FunctionRegistryActor is available
      if (!this.functionRegistry) {
        return this.jsonResponse(
          { error: 'Function registry not available' },
          503,
          corsHeaders
        );
      }

      // Build filter options
      const filters = {};
      if (params.has('type')) {
        filters.type = params.get('type');
      }

      // List functions
      const result = this.functionRegistry.listFunctions(filters);

      return this.jsonResponse(
        {
          success: true,
          functions: result.data.functions,
          count: result.data.count
        },
        200,
        corsHeaders
      );
    } catch (error) {
      throw error;
    }
  }

  /**
   * GET /patterns - List registered patterns
   */
  async handleGetPatterns(request, corsHeaders) {
    try {
      const url = new URL(request.url);
      const params = url.searchParams;

      // Check if PatternMatcherActor is available
      if (!this.patternMatcher) {
        return this.jsonResponse(
          { error: 'Pattern matcher not available' },
          503,
          corsHeaders
        );
      }

      // Build options
      const options = {
        sortByPriority: params.has('sortByPriority')
          ? params.get('sortByPriority') === 'true'
          : false
      };

      // List patterns
      const patterns = this.patternMatcher.listPatterns(options);

      return this.jsonResponse(
        {
          success: true,
          patterns: patterns,
          count: patterns.length
        },
        200,
        corsHeaders
      );
    } catch (error) {
      throw error;
    }
  }

  /**
   * GET /health - Health check
   */
  async handleGetHealth(request, corsHeaders) {
    const uptime = this.startTime
      ? Date.now() - new Date(this.startTime).getTime()
      : 0;

    return this.jsonResponse(
      {
        status: 'ok',
        uptime,
        startTime: this.startTime,
        actors: {
          eventLog: this.eventLog ? 'available' : 'unavailable',
          functionRegistry: this.functionRegistry ? 'available' : 'unavailable',
          patternMatcher: this.patternMatcher ? 'available' : 'unavailable'
        }
      },
      200,
      corsHeaders
    );
  }

  /**
   * Helper to create JSON responses
   */
  jsonResponse(data, status = 200, additionalHeaders = {}) {
    return new Response(JSON.stringify(data, null, 2), {
      status,
      headers: {
        'Content-Type': 'application/json',
        ...additionalHeaders
      }
    });
  }

  /**
   * Stop the HTTP server gracefully
   */
  async close() {
    if (!this.isRunning || !this.server) {
      return { success: true };
    }

    try {
      this.server.stop();
      this.isRunning = false;
      console.log('[HTTPServerActor] Server stopped');

      return { success: true };
    } catch (error) {
      return {
        success: false,
        error: `Failed to stop server: ${error.message}`
      };
    }
  }

  /**
   * Get server status
   */
  getStatus() {
    return {
      isRunning: this.isRunning,
      port: this.port,
      host: this.host,
      url: this.isRunning ? `http://${this.host}:${this.port}` : null,
      startTime: this.startTime,
      uptime: this.startTime
        ? Date.now() - new Date(this.startTime).getTime()
        : 0
    };
  }
}

/**
 * Factory function to create HTTPServerActor instance
 */
export function createHTTPServerActor(config) {
  return new HTTPServerActor(config);
}

export default HTTPServerActor;
