#!/usr/bin/env bun
/**
 * DaemonActor - Main orchestrator for the event system
 *
 * Responsibilities:
 * - Lifecycle management (start/stop/status)
 * - Configuration loading
 * - Actor spawning (EventLogActor)
 * - Graceful shutdown handling
 * - Signal handling (SIGINT/SIGTERM)
 */

import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';

/**
 * Daemon state
 */
const STATE = {
  STOPPED: 'stopped',
  STARTING: 'starting',
  RUNNING: 'running',
  STOPPING: 'stopping',
  ERROR: 'error'
};

/**
 * DaemonActor class
 */
export class DaemonActor {
  constructor(configPath = './config.json') {
    this.configPath = configPath;
    this.config = null;
    this.state = STATE.STOPPED;
    this.actors = {
      eventLog: null,
      // Future actors will be added here
    };
    this.shutdownHandlers = [];
    this.startTime = null;
    this.error = null;
  }

  /**
   * Load configuration from file
   */
  async loadConfig() {
    try {
      const configFile = resolve(this.configPath);
      const configData = await readFile(configFile, 'utf-8');
      this.config = JSON.parse(configData);
      return { success: true, config: this.config };
    } catch (error) {
      return {
        success: false,
        error: `Failed to load config: ${error.message}`
      };
    }
  }

  /**
   * Start the daemon
   */
  async start() {
    if (this.state !== STATE.STOPPED) {
      return {
        success: false,
        error: `Cannot start daemon in ${this.state} state`
      };
    }

    this.state = STATE.STARTING;
    this.startTime = new Date().toISOString();

    try {
      // Load configuration
      const configResult = await this.loadConfig();
      if (!configResult.success) {
        this.state = STATE.ERROR;
        this.error = configResult.error;
        return configResult;
      }

      console.log('[DaemonActor] Configuration loaded:', this.config);

      // Spawn EventLogActor
      await this.spawnEventLogActor();

      // Set up signal handlers
      this.setupSignalHandlers();

      // Mark as running
      this.state = STATE.RUNNING;
      console.log('[DaemonActor] Daemon started successfully');
      console.log(`[DaemonActor] Process ID: ${process.pid}`);
      console.log(`[DaemonActor] Config: ${this.configPath}`);

      return { success: true, state: this.state };
    } catch (error) {
      this.state = STATE.ERROR;
      this.error = error.message;
      console.error('[DaemonActor] Failed to start:', error);
      return {
        success: false,
        error: error.message
      };
    }
  }

  /**
   * Spawn EventLogActor
   * Note: This is a placeholder until EventLogActor is implemented
   */
  async spawnEventLogActor() {
    try {
      // TODO: Import and instantiate EventLogActor when implemented
      // For now, we'll create a placeholder
      console.log('[DaemonActor] EventLogActor spawn requested');

      // Check if EventLogActor module exists
      try {
        const { EventLogActor } = await import('./actors/event-log.js');
        this.actors.eventLog = new EventLogActor(this.config);
        await this.actors.eventLog.initialize();
        console.log('[DaemonActor] EventLogActor spawned successfully');
      } catch (error) {
        // EventLogActor not yet implemented - continue without it
        console.log('[DaemonActor] EventLogActor not yet implemented, skipping spawn');
        this.actors.eventLog = {
          state: 'not_implemented',
          stop: async () => { /* noop */ }
        };
      }

      return { success: true };
    } catch (error) {
      console.error('[DaemonActor] Failed to spawn EventLogActor:', error);
      throw error;
    }
  }

  /**
   * Set up signal handlers for graceful shutdown
   */
  setupSignalHandlers() {
    const signals = ['SIGINT', 'SIGTERM'];

    signals.forEach(signal => {
      process.on(signal, async () => {
        console.log(`\n[DaemonActor] Received ${signal}, initiating graceful shutdown...`);
        await this.stop();
        process.exit(0);
      });
    });

    // Handle uncaught exceptions
    process.on('uncaughtException', async (error) => {
      console.error('[DaemonActor] Uncaught exception:', error);
      this.state = STATE.ERROR;
      this.error = error.message;
      await this.stop();
      process.exit(1);
    });

    // Handle unhandled promise rejections
    process.on('unhandledRejection', async (reason, promise) => {
      console.error('[DaemonActor] Unhandled rejection at:', promise, 'reason:', reason);
      this.state = STATE.ERROR;
      this.error = String(reason);
      await this.stop();
      process.exit(1);
    });
  }

  /**
   * Stop the daemon gracefully
   */
  async stop() {
    if (this.state === STATE.STOPPED || this.state === STATE.STOPPING) {
      return { success: true, state: this.state };
    }

    this.state = STATE.STOPPING;
    console.log('[DaemonActor] Stopping daemon...');

    try {
      // Stop all actors
      if (this.actors.eventLog) {
        console.log('[DaemonActor] Stopping EventLogActor...');
        await this.actors.eventLog.stop();
      }

      // Run custom shutdown handlers
      for (const handler of this.shutdownHandlers) {
        try {
          await handler();
        } catch (error) {
          console.error('[DaemonActor] Shutdown handler error:', error);
        }
      }

      this.state = STATE.STOPPED;
      console.log('[DaemonActor] Daemon stopped successfully');

      return { success: true, state: this.state };
    } catch (error) {
      console.error('[DaemonActor] Error during shutdown:', error);
      this.state = STATE.ERROR;
      this.error = error.message;
      return {
        success: false,
        error: error.message
      };
    }
  }

  /**
   * Get daemon status
   */
  getStatus() {
    const uptime = this.startTime
      ? Date.now() - new Date(this.startTime).getTime()
      : 0;

    return {
      state: this.state,
      pid: process.pid,
      uptime,
      startTime: this.startTime,
      error: this.error,
      config: this.config,
      actors: {
        eventLog: this.actors.eventLog
          ? (this.actors.eventLog.state || 'running')
          : 'not_spawned'
      }
    };
  }

  /**
   * Register a shutdown handler
   */
  onShutdown(handler) {
    this.shutdownHandlers.push(handler);
  }
}

/**
 * Main entry point when run directly
 */
if (import.meta.main) {
  const daemon = new DaemonActor();

  // Start the daemon
  const result = await daemon.start();

  if (!result.success) {
    console.error('[DaemonActor] Failed to start daemon:', result.error);
    process.exit(1);
  }

  // Log status every 10 seconds (optional, for debugging)
  if (process.env.DEBUG) {
    setInterval(() => {
      const status = daemon.getStatus();
      console.log('[DaemonActor] Status:', JSON.stringify(status, null, 2));
    }, 10000);
  }

  // Keep process alive
  console.log('[DaemonActor] Daemon running. Press CTRL+C to stop.');
}

export default DaemonActor;
